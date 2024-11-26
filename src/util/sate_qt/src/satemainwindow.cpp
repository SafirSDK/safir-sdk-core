/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include "satemainwindow.h"
#include "ui_satemainwindow.h"
#include "typesystemrepository.h"
#include "dobobjecteditwidget.h"
#include "dobhandler.h"
#include "dobwebsocket.h"
#include "instanceswidget.h"
#include "receivedmodel.h"
#include "typesystemwidget.h"
#include "connectdialog.h"

#include <QtConcurrent/QtConcurrent>
#include <QLabel>
#include <QTabWidget>
#include <QTableView>
#include <QHeaderView>
#include <QCloseEvent>
#include <QMessageBox>
#include <QDateTime>
#include <QScrollBar>

#include <QTextBrowser>
#include <QFile>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>

#include <Safir/Utilities/Internal/Expansion.h>

SateMainWindow::SateMainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::SateMainWindow)
    , m_dob()
    , m_connectDialog(new ConnectDialog(this))
{
    ui->setupUi(this);

    ads::CDockManager::setConfigFlag(ads::CDockManager::DockAreaHasUndockButton, false);
    ads::CDockManager::setConfigFlag(ads::CDockManager::DockAreaHasCloseButton, false);
    ads::CDockManager::setConfigFlag(ads::CDockManager::MiddleMouseButtonClosesTab, true);
    ads::CDockManager::setConfigFlag(ads::CDockManager::TabCloseButtonIsToolButton, true);
    //ads::CDockManager::setAutoHideConfigFlags(ads::CDockManager::DefaultAutoHideConfig);

    m_dockManager = new ads::CDockManager(this);
    m_dockManager->setStyleSheet("");

    //now we have everything we need to set the initial stylesheets
    OnDarkMode();

    //Set up the central area as always present
    QLabel* label = new QLabel();
    label->setText("Welcome to the next generation Sate. SateNext!");
    label->setAlignment(Qt::AlignCenter);
    auto* centralDockWidget = new ads::CDockWidget("CentralWidget");
    centralDockWidget->setWidget(label);
    centralDockWidget->setFeature(ads::CDockWidget::NoTab, true);
    m_centralDockArea = m_dockManager->setCentralWidget(centralDockWidget);

    m_typesystem = new TypesystemWidget(this);
    m_typesystem->Initialize(&m_dob);

    auto* typesystemDock = new ads::CDockWidget("Typesystem");
    typesystemDock->setWidget(m_typesystem);
    m_dockManager->addDockWidget(ads::LeftDockWidgetArea, typesystemDock);
    ui->menuView->addAction(typesystemDock->toggleViewAction());

    m_instanceLabel = new QLabel(tr("SAFIR_INSTANCE: %1").arg(Safir::Utilities::Internal::Expansion::GetSafirInstance()));
    m_connectionStringLabel = new QLabel();
    ui->statusbar->addPermanentWidget(m_connectionStringLabel);
    ui->statusbar->addPermanentWidget(m_instanceLabel);
    OnInfo("Trying to connect to DOB...", QtInfoMsg);

    // TypesystemWidget signal handling
    connect(m_typesystem, &TypesystemWidget::OpenObjectEdit, this, &SateMainWindow::OnOpenObjectEdit);
    connect(m_typesystem, &TypesystemWidget::OpenEntityInstanceViewer, this, &SateMainWindow::OnOpenEntityInstanceViewer);
    connect(m_typesystem, &TypesystemWidget::OpenMessageInstanceViewer, this, &SateMainWindow::OnOpenMessageInstanceViewer);
    connect(m_typesystem, &TypesystemWidget::OpenDouFile, this, &SateMainWindow::OnOpenDouFile);

    // Received table
    m_received = new QTableView();
    m_received->setModel(new ReceivedModel(&m_dob, m_received));
    m_received->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    m_received->setColumnWidth(1, 250);
    m_received->horizontalHeader()->setSectionResizeMode(3, QHeaderView::ResizeToContents);
    m_received->setColumnWidth(3, 250);
    m_received->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    m_received->horizontalHeader()->setDefaultAlignment(Qt::AlignLeft);
    m_received->setSelectionBehavior(QAbstractItemView::SelectRows);

    connect(m_received, &QTableView::doubleClicked, this, &SateMainWindow::OnReceivedTableDoubleClicked);

    auto* receivedDock = new ads::CDockWidget("Received");
    receivedDock->setWidget(m_received);
    m_dockManager->addDockWidgetTab(ads::BottomDockWidgetArea, receivedDock);
    ui->menuView->addAction(receivedDock->toggleViewAction());

    // Output window
    m_output = new QTextBrowser(this);
#if 0
    m_output->setReadOnly(true);
    m_output->setUndoRedoEnabled(false);
    m_output->setAcceptRichText(false);
    m_output->setOpenLinks(false);
#endif
    auto* outputDock = new ads::CDockWidget("Output", this);
    outputDock->setWidget(m_output);
    m_dockManager->addDockWidgetTab(ads::BottomDockWidgetArea, outputDock);
    ui->menuView->addAction(outputDock->toggleViewAction());
    connect(&m_dob, &DobHandler::Info, this, &SateMainWindow::OnInfo);

    // Connection menu
    connect(ui->actionDisconnect, &QAction::triggered, this, [this]{ m_dob.Close(); });
    connect(ui->actionConnect, &QAction::triggered, this, [this]{ m_connectDialog->show(); });
    connect(m_connectDialog, &QDialog::accepted, this, [this]
    {
        if (m_connectDialog->NativeConnection())
        {
            m_dob.OpenNativeConnection(m_connectDialog->ConnectionName(), m_connectDialog->Context());
        }
        else // websocket connection
        {
            m_dob.OpenWebsocketConnection(m_connectDialog->WsAddress(), m_connectDialog->WsPort(), m_connectDialog->ConnectionName(), m_connectDialog->Context());
        }
    });

    // Style sheet menu
    connect(ui->actionDarkMode, &QAction::triggered, this, &SateMainWindow::OnDarkMode);
    connect(ui->actionLightMode, &QAction::triggered, this, &SateMainWindow::OnLightMode);

    // DOB signal handling
    connect(&m_dob, &DobHandler::ConnectedToDob, this, &SateMainWindow::OnConnectedToDob);
    m_dob.OpenNativeConnection("SATE", 0);
}

SateMainWindow::~SateMainWindow()
{
    delete ui;

    for (auto&& dock: m_dockManager->dockWidgetsMap())
    {
        m_dockManager->removeDockWidget(dock);
    }

    if (!m_connected)
    {
        //If the connect is still pending we need to kill the program very forcefully, since there is no way
        //to interrupt the Open call.
        std::quick_exit(0);
    }
}

void SateMainWindow::OnOpenEntityInstanceViewer(const int64_t typeId, const bool includeSubclasses)
{
    OpenInstanceViewer(typeId, includeSubclasses,
                       [this, typeId, includeSubclasses]
                       {return new InstancesWidget(&m_dob, typeId, includeSubclasses, this);});
}

void SateMainWindow::OnOpenMessageInstanceViewer(const int64_t typeId,
                                                 const Safir::Dob::Typesystem::ChannelId& channel,
                                                 const bool includeSubclasses)
{
    OpenInstanceViewer(typeId, includeSubclasses,
                       [this, typeId, channel, includeSubclasses]
                       {return new InstancesWidget(&m_dob, typeId, channel, includeSubclasses, this);});
}

void SateMainWindow::OpenInstanceViewer(const int64_t typeId,
                                        const bool includeSubclasses,
                                        const std::function<InstancesWidget* ()>& factory)
{
    const auto* const cls = TypesystemRepository::Instance().GetClass(typeId);
    const auto tabObjectName = "IV" + cls->name;
    auto* dock = m_dockManager->findDockWidget(tabObjectName);
    if (dock != nullptr)
    {
        if (dock->property("includeSubclasses").toBool() != includeSubclasses)
        {
            m_dockManager->removeDockWidget(dock);
            OnInfo("Closed a previous viewer for the same type but with different recursiveness.", QtWarningMsg);
        }
        else
        {
            dock->raise();
            return;
        }
    }

    // no previous dock found, create new
    auto* iv = factory();
    dock = new ads::CDockWidget(cls->name);
    dock->setObjectName(tabObjectName);
    dock->setWidget(iv, ads::CDockWidget::ForceNoScrollArea);
    dock->setFeature(ads::CDockWidget::DockWidgetDeleteOnClose, true);
    dock->setProperty("includeSubclasses",includeSubclasses);
    if (includeSubclasses)
    {
        dock->setIcon(QIcon(":/img/icons/instance_viewer_recursive.png"));
    }
    else
    {
        dock->setIcon(QIcon(":/img/icons/instance_viewer.png"));
    }

    if (cls->dobBaseClass == TypesystemRepository::Entity)
    {
        if (includeSubclasses)
        {
            dock->setTabToolTip(tr("All instances of the entity %1 and its subclasses").arg(cls->name));
        }
        else
        {
            dock->setTabToolTip(tr("All instances of the entity %1").arg(cls->name));
        }
    }
    else if (cls->dobBaseClass == TypesystemRepository::Message)
    {
        if (includeSubclasses)
        {
            dock->setTabToolTip(tr("The most recently received instances of the message %1 and its subclasses")
                                .arg(cls->name));
        }
        else
        {
            dock->setTabToolTip(tr("The most recently received instances of the message %1").arg(cls->name));
        }
    }
    else
    {
        throw std::logic_error("Need to add an approprate tooltip for this type");
    }

    m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
    connect(iv,&InstancesWidget::OpenObjectEdit,this,&SateMainWindow::OnOpenObjectEditWithInstance);
}

void SateMainWindow::OnConnectedToDob(const QString& connectionName)
{
    QString msg = tr("Connected as %1").arg(connectionName);
    OnInfo(msg,QtInfoMsg);
    m_connectionStringLabel->setText(connectionName);
    m_connectionStringLabel->setToolTip(tr("Connection name of this instance of Sate"));
    m_connected = true;
}

void SateMainWindow::OnReceivedTableDoubleClicked(const QModelIndex& ix)
{
    if (!ix.isValid())
    {
        return;
    }

    const auto recvObjItem =
        static_cast<ReceivedModel*>(m_received->model())->ReadItem(ix.row());

    if (recvObjItem.object)
    {
        OnOpenObjectEditWithInstance(recvObjItem.typeId,
                                     recvObjItem.channelHandler,
                                     recvObjItem.instance,
                                     recvObjItem.object);
    }
}

void SateMainWindow::OnOpenObjectEdit(const int64_t typeId)
{
    auto oe = new DobObjectEditWidget(&m_dob, typeId, this);
    connect(oe, &DobObjectEditWidget::XmlSerializedObject, this, &SateMainWindow::AddXmlPage);
    connect(oe, &DobObjectEditWidget::JsonSerializedObject, this, &SateMainWindow::AddJsonPage);
    AddTab(TypesystemRepository::Instance().GetClass(typeId)->name, oe);
}

void SateMainWindow::OnOpenObjectEditWithInstance(int64_t typeId,
                                                  QString channelHandler,
                                                  int64_t instance,
                                                  const Safir::Dob::Typesystem::ObjectPtr& object)
{
    auto oe = new DobObjectEditWidget(&m_dob, typeId, channelHandler, instance, object, this);
    connect(oe, &DobObjectEditWidget::XmlSerializedObject, this, &SateMainWindow::AddXmlPage);
    connect(oe, &DobObjectEditWidget::JsonSerializedObject, this, &SateMainWindow::AddJsonPage);
    AddTab(TypesystemRepository::Instance().GetClass(typeId)->name, oe);
}

void SateMainWindow::OnOpenDouFile(const int64_t typeId)
{
    auto path = QString::fromStdWString(sdt::Internal::GetDouFilePath(typeId));
    QFile f(path);
    if (!f.open(QFile::ReadOnly | QFile::Text))
    {
        QMessageBox mb;
        mb.setText("Could not open dou file: " + path);
        mb.exec();
        return;
    }
    QTextStream in(&f);
    auto text = in.readAll();
    AddXmlPage(path, text);
}

void SateMainWindow::AddXmlPage(const QString& title, const QString& text)
{
    QString xmlFormatted;
    QXmlStreamReader reader(text);
    reader.setNamespaceProcessing(false);

    QXmlStreamWriter writer(&xmlFormatted);
    writer.setAutoFormatting(true);

    while (!reader.atEnd())
    {
        reader.readNext();
        if (!reader.isWhitespace()) {
            writer.writeCurrentToken(reader);
        }
    }

    auto textBrowser = new  QTextBrowser();
    textBrowser->setPlainText(xmlFormatted);
    AddTab(title, textBrowser);
}

void SateMainWindow::AddJsonPage(const QString& title, const QString& text)
{
    QJsonDocument doc = QJsonDocument::fromJson(text.toUtf8());
    QString formattedJsonString = doc.toJson(QJsonDocument::Indented);
    auto textBrowser = new  QTextBrowser();
    textBrowser->setPlainText(formattedJsonString);
    AddTab(title, textBrowser);
}

void SateMainWindow::AddTab(const QString& title, QWidget* widget)
{
    auto* dock = new ads::CDockWidget(title);
    widget->setParent(dock);
    dock->setWidget(widget);
    dock->setFeature(ads::CDockWidget::DockWidgetDeleteOnClose, true);
    m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
}

void SateMainWindow::OnDarkMode()
{
    ui->actionDarkMode->setChecked(true);
    ui->actionLightMode->setChecked(false);

    QFile ds(":qdarkstyle/dark/darkstyle.qss");
    QFile tweaks(":customizations/tweaks.qss");
    if (ds.exists() && tweaks.exists())
    {
        ds.open(QFile::ReadOnly | QFile::Text);
        tweaks.open(QFile::ReadOnly | QFile::Text);
        QTextStream ts1(&ds);
        QTextStream ts2(&tweaks);
        qApp->setStyleSheet(ts1.readAll() + "\n" + ts2.readAll());
    }

    QFile ads(":customizations/ads-dark.qss");
    if (ads.exists())
    {
        ads.open(QFile::ReadOnly | QFile::Text);
        QTextStream ts(&ads);
        m_dockManager->setStyleSheet(ts.readAll());
    }

}

void SateMainWindow::OnLightMode()
{
    ui->actionDarkMode->setChecked(false);
    ui->actionLightMode->setChecked(true);

    QFile ds(":qdarkstyle/light/lightstyle.qss");
    QFile tweaks(":customizations/tweaks.qss");
    if (ds.exists() && tweaks.exists())
    {
        ds.open(QFile::ReadOnly | QFile::Text);
        tweaks.open(QFile::ReadOnly | QFile::Text);
        QTextStream ts1(&ds);
        QTextStream ts2(&tweaks);
        qApp->setStyleSheet(ts1.readAll() + "\n" + ts2.readAll());
    }

    QFile ads(":customizations/ads-light.qss");
    if (ads.exists())
    {
        ads.open(QFile::ReadOnly | QFile::Text);
        QTextStream ts(&ads);
        m_dockManager->setStyleSheet(ts.readAll());
    }
}

void SateMainWindow::OnInfo(const QString& info, const QtMsgType msgType)
{
    const bool startTimer = m_pendingOutput.empty();

    QStringList line;
    line += "<i style='color:grey'>" + QDateTime::currentDateTime().toString("hh:mm:ss") + "</i>: ";
    switch (msgType)
    {
    case QtDebugMsg:
    case QtInfoMsg:
        break;
    case QtWarningMsg:
        line+="<b>";
        break;
    case QtCriticalMsg:
    case QtFatalMsg:
        line+="<span style='color:red'>";
        break;
    }

    line += info;

        switch (msgType)
    {
    case QtDebugMsg:
    case QtInfoMsg:
        break;
    case QtWarningMsg:
        line+="</b>";
        break;
    case QtCriticalMsg:
    case QtFatalMsg:
        line+="</span>";
        break;
    }

    line += "<br>";

    m_pendingOutput += line.join("");

    if (startTimer)
    {
        QTimer::singleShot(std::chrono::milliseconds(200),
                           [this]
                           {
                               m_output->setUpdatesEnabled(false);
                               m_output->insertHtml(m_pendingOutput.join("\n"));
                               m_output->setUpdatesEnabled(true);
                               m_pendingOutput.clear();
                               auto *sb = m_output->verticalScrollBar();
                               sb->setValue(sb->maximum());
                           });
    }
}
