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
    ui->statusbar->addPermanentWidget(m_instanceLabel);
    ui->statusbar->showMessage(tr("Trying to connect to DOB..."), std::numeric_limits<int>::max());

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
    auto* outputDock = new ads::CDockWidget("Output", this);
    outputDock->setWidget(m_output);
    m_dockManager->addDockWidgetTab(ads::BottomDockWidgetArea, outputDock);
    ui->menuView->addAction(outputDock->toggleViewAction());
    connect(&m_dob, &DobHandler::Info, this, &SateMainWindow::OnInfo);

    // Connection menu
    connect(ui->actionDisconnect, &QAction::triggered, this, [this]{ m_dob.Close(); });
    connect(ui->actionConnect, &QAction::triggered, this, [this]{ m_dob.OpenNativeConnection("SATE", 0); });
    connect(ui->actionConnectWs, &QAction::triggered, this, [this]{ m_dob.OpenWebsocketConnection("localhost", 10000, "SATE", 0); });

    // Connection menu
    connect(ui->actionConnect, &QAction::triggered, this, [this]{ m_dob.Close(); });

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
            ui->statusbar->showMessage("Closed a previous viewer for the same type but with different recursiveness.", 30000);
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
    dock->setWidget(iv);
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
    ui->statusbar->showMessage(msg, 30000);
    m_instanceLabel->setToolTip(msg);
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

void SateMainWindow::OnInfo(const QString& info)
{
    auto time = "<i style='color:grey'>" + QDateTime::currentDateTime().toString("hh:mm:ss") + "</i>: ";
    m_output->insertHtml(time + info + "<br>");
    auto *sb = m_output->verticalScrollBar();
    sb->setValue(sb->maximum());
}
