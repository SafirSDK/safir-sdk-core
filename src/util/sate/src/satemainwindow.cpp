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
#include "outputwidget.h"
#include "instanceswidget.h"
#include "typesystemwidget.h"
#include "connectdialog.h"
#include "parameterswidget.h"
#include "enumwidget.h"

#include <QCloseEvent>
#include <QActionGroup>
#include <QDateTime>
#include <QDebug>
#include <QFile>
#include <QHeaderView>
#include <QLabel>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QPushButton>
#include <QScrollBar>
#include <QTabWidget>
#include <QTableView>
#include <QTextBrowser>
#include <QToolBar>
#include <QToolButton>
#include <QFileDialog>
#include <QtConcurrent/QtConcurrent>

#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Dob/Typesystem/Serialization.h>

#include <Safir/Utilities/Internal/Expansion.h>

SateMainWindow::SateMainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::SateMainWindow)
    , m_toolBar(new QToolBar(this))
    , m_connectedLabel(new QLabel())
    , m_dob()
    , m_connectDialog(new ConnectDialog(this))
{
    ui->setupUi(this);
    setAcceptDrops(true);

    ads::CDockManager::setConfigFlag(ads::CDockManager::DockAreaHasUndockButton, false);
    ads::CDockManager::setConfigFlag(ads::CDockManager::DockAreaHasCloseButton, false);
    ads::CDockManager::setConfigFlag(ads::CDockManager::MiddleMouseButtonClosesTab, true);
    ads::CDockManager::setConfigFlag(ads::CDockManager::TabCloseButtonIsToolButton, true);
    ads::CDockManager::setConfigFlag(ads::CDockManager::FocusHighlighting, true);

    //Set up the combined toolbar and menubar
    addToolBar(Qt::TopToolBarArea, m_toolBar);
    m_toolBar->setFloatable(false);
    m_toolBar->setMovable(false);
    m_toolBar->addWidget(ui->menubar);

    m_dockManager = new ads::CDockManager(this);

    //now we have everything we need to set the initial stylesheets
    m_dockManager->setStyleSheet("");
    OnThemeChanged();

    //Set up the central area as always present
    QLabel* label = new QLabel();
    label->setText("Welcome to the next generation Sate, rewritten in Qt.\n"
                   "Please report any bugs you find, so we can make this the best Sate ever!\n\n"
                   "If you have good ideas about appropriate features that will make Sate better\n"
                   "to use for systems debugging, please pass them on to us!\n\n"
                   "The old Sate is still available, now renamed to sate_legacy.");
    label->setAlignment(Qt::AlignCenter);
    auto* centralDockWidget = new ads::CDockWidget("CentralWidget");
    centralDockWidget->setWidget(label);
    centralDockWidget->setFeature(ads::CDockWidget::NoTab, true);
    centralDockWidget->setFeature(ads::CDockWidget::DockWidgetFocusable, false);
    m_centralDockArea = m_dockManager->setCentralWidget(centralDockWidget);

    m_typesystem = new TypesystemWidget(this);
    m_typesystem->Initialize(&m_dob);

    auto* typesystemDock = new ads::CDockWidget("Typesystem");
    typesystemDock->setWidget(m_typesystem, ads::CDockWidget::ForceNoScrollArea);
    typesystemDock->setFeature(ads::CDockWidget::DockWidgetFocusable, false);
    m_dockManager->addDockWidget(ads::LeftDockWidgetArea, typesystemDock);
    typesystemDock->toggleViewAction()->setShortcut(QKeySequence(tr("Ctrl+T")));;

    m_instanceLabel = new QLabel(tr("SAFIR_INSTANCE: %1").arg(Safir::Utilities::Internal::Expansion::GetSafirInstance()));
    ui->statusbar->addPermanentWidget(m_connectedLabel);
    ui->statusbar->addPermanentWidget(m_instanceLabel);
    OnConnectionClosed(); //set the status bar correctly

    // TypesystemWidget signal handling
    connect(m_typesystem, &TypesystemWidget::OpenObjectEdit, this, &SateMainWindow::OnOpenObjectEdit);
    connect(m_typesystem, &TypesystemWidget::OpenEntityInstanceViewer, this, &SateMainWindow::OnOpenEntityInstanceViewer);
    connect(m_typesystem, &TypesystemWidget::OpenMessageInstanceViewer, this, &SateMainWindow::OnOpenMessageInstanceViewer);
    connect(m_typesystem, &TypesystemWidget::OpenParameterViewer, this, &SateMainWindow::OnOpenParameterViewer);
    connect(m_typesystem, &TypesystemWidget::OpenEnumViewer, this, &SateMainWindow::OnOpenEnumViewer);
    connect(m_typesystem, &TypesystemWidget::OpenDouFile, this, &SateMainWindow::OnOpenDouFile);

    // Output window
    m_output = new OutputWidget(&m_dob, this);
    auto* outputDock = new ads::CDockWidget("Output", this);
    outputDock->setWidget(m_output, ads::CDockWidget::ForceNoScrollArea);
    outputDock->setFeature(ads::CDockWidget::DockWidgetFocusable, false);
    m_dockManager->addDockWidgetTab(ads::BottomDockWidgetArea, outputDock);
    outputDock->toggleViewAction()->setShortcut(QKeySequence(tr("Ctrl+L")));;
    connect(m_output ,&OutputWidget::OpenObjectEdit, this, &SateMainWindow::OnOpenObjectEditWithInstance);

    // Connection menu
    connect(ui->actionDisconnect, &QAction::triggered, this, [this]{ m_dob.Close(); });
    connect(ui->actionConnect, &QAction::triggered, this, [this]{ m_connectDialog->show(); });
    connect(ui->actionOpenObject, &QAction::triggered, this, [this]{
        OpenSerializedObject(QFileDialog::getOpenFileName(this, "Deserialize Object", "", "XML/JSON files (*.xml *.json);;Any file (*.*)"));
    });
    connect(ui->actionQuit, &QAction::triggered, this, []{QApplication::quit();});
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

    auto* themeGroup = new QActionGroup(this);
    themeGroup->addAction(ui->actionDarkMode);
    themeGroup->addAction(ui->actionLightMode);
    themeGroup->setExclusive(true);

    connect(themeGroup, &QActionGroup::triggered, this, &SateMainWindow::OnThemeChanged);
    connect(ui->actionTouchMode, &QAction::triggered, this, &SateMainWindow::OnThemeChanged);

    // DOB signal handling
    connect(&m_dob, &DobHandler::ConnectedToDob, this, &SateMainWindow::OnConnectedToDob);
    connect(&m_dob, &DobHandler::ConnectionClosed, this, &SateMainWindow::OnConnectionClosed);
    connect(&m_dob, &DobHandler::OnReadEntity, this, [this](const auto& entity, const auto& inst){ OnOpenObjectEditWithInstance("", inst.GetRawValue(), entity); });
    m_dob.OpenNativeConnection("SATE", 0);

    connect(m_dockManager,&ads::CDockManager::focusedDockWidgetChanged,this,&SateMainWindow::OnFocusedDockWidgetChanged);

    auto* closeCurrentTabAction = new QAction("Close current tab");
    closeCurrentTabAction->setShortcut(QKeySequence(tr("Ctrl+W")));;
    closeCurrentTabAction->setToolTip("Close the currently active tab");
    connect(closeCurrentTabAction, &QAction::triggered, this, &SateMainWindow::OnCloseCurrentTab);

    auto* closeAllTabsAction = new QAction("Close all tabs");
    closeAllTabsAction->setShortcut(QKeySequence(tr("Ctrl+Shift+W")));;
    closeAllTabsAction->setToolTip("Close all open tabs");
    connect(closeAllTabsAction, &QAction::triggered, this, &SateMainWindow::OnCloseAllTabs);

    auto* showTypeInTreeAction = new QAction(QIcon(":/img/icons/magnifying-glass-location-solid.svg"),"Find tab type in tree");
    showTypeInTreeAction->setShortcut(QKeySequence(tr("Ctrl+F")));;
    showTypeInTreeAction->setToolTip("Find the type of the current tab in the typesystem tree view.");
    connect(showTypeInTreeAction, &QAction::triggered, this, &SateMainWindow::OnFindType);

    m_midnightCommanderModeAction = new QAction(QIcon(":/img/icons/moon-solid.svg"), "Midnight Commander Mode");
    m_midnightCommanderModeAction->setShortcut(QKeySequence(tr("Ctrl+M")));;
    m_midnightCommanderModeAction->setCheckable(true);
    m_midnightCommanderModeAction->setToolTip("Enable Midnight Commander Mode\n\n"
                                              "Opens object edit views that are clicked on inside\n"
                                              "instance views open in a separate tab group.");
    connect(m_midnightCommanderModeAction, &QAction::triggered, this, &SateMainWindow::OnMidnightCommanderToggled);

    m_resetWindowsAction = new QAction(QIcon(":/img/icons/window_reset.png"),"Reset layout");
    m_resetWindowsAction->setShortcut(QKeySequence(tr("Ctrl+R")));;
    m_resetWindowsAction->setToolTip("Move all tabs back to default positions");
    connect(m_resetWindowsAction, &QAction::triggered, this, &SateMainWindow::OnResetWindows);

    m_toolBar->addAction(showTypeInTreeAction);
    m_toolBar->addAction(m_midnightCommanderModeAction);
    m_toolBar->addAction(m_resetWindowsAction);

    ui->menuView->addAction(m_midnightCommanderModeAction);
    ui->menuView->addAction(m_resetWindowsAction);
    ui->menuView->addAction(showTypeInTreeAction);
    ui->menuView->addAction(closeCurrentTabAction);
    ui->menuView->addAction(closeAllTabsAction);
    ui->menuView->addSeparator();
    ui->menuView->addAction(typesystemDock->toggleViewAction());
    ui->menuView->addAction(outputDock->toggleViewAction());

    m_typesystem->SetSearchFocus();
}

SateMainWindow::~SateMainWindow()
{
    delete ui;

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
            m_output->Output("Closed a previous viewer for the same type but with different recursiveness.", QtWarningMsg);
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
        dock->setIcon(QIcon(":/img/icons/eye-solid-red.svg"));
    }
    else
    {
        dock->setIcon(QIcon(":/img/icons/eye-solid.svg"));
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
    connect(iv,&InstancesWidget::ReadEntity,this,
            [this](const Safir::Dob::Typesystem::EntityId& entityId){m_dob.ReadEntity(entityId);});
}

void SateMainWindow::OnConnectedToDob(const QString& connectionName)
{
    QString msg = tr("Connected as %1").arg(connectionName);
    m_output->Output(msg, QtInfoMsg);
    m_connectedLabel->setText(tr("Connected"));
    m_connectedLabel->setObjectName("ConnectedLabelConnected");
    m_connectedLabel->setToolTip(tr("Connection name: %1").arg(connectionName));
    m_connected = true;

    style()->unpolish(m_connectedLabel);
    style()->polish(m_connectedLabel);
}

void SateMainWindow::OnConnectionClosed()
{
    m_connectedLabel->setText(tr("Not connected"));
    m_connectedLabel->setObjectName("ConnectedLabelDisconnected");
    m_connectedLabel->setToolTip("");
    m_connected = false;

    style()->unpolish(m_connectedLabel);
    style()->polish(m_connectedLabel);

}

void SateMainWindow::OnOpenObjectEdit(const int64_t typeId)
{
    auto oe = new DobObjectEditWidget(&m_dob, typeId, this);
    connect(oe, &DobObjectEditWidget::XmlSerializedObject, this, &SateMainWindow::AddXmlPage);
    connect(oe, &DobObjectEditWidget::JsonSerializedObject, this, &SateMainWindow::AddJsonPage);
    AddTab(TypesystemRepository::Instance().GetClass(typeId)->name,"OE", oe, false);
}

void SateMainWindow::OnOpenParameterViewer(const int64_t typeId, const QString& currentItem)
{
    auto paramWidget = new ParametersWidget(typeId, currentItem, this);
    AddTab(TypesystemRepository::Instance().GetClass(typeId)->name, "PV", paramWidget, false);
}

void SateMainWindow::OnOpenEnumViewer(const int64_t typeId, const QString& currentItem)
{
    auto enumWidget = new EnumWidget(typeId, currentItem, this);
    AddTab(TypesystemRepository::Instance().GetEnum(typeId)->name, "EN", enumWidget, false);
}

void SateMainWindow::OnOpenObjectEditWithInstance(QString channelHandler,
                                                  int64_t instance,
                                                  const Safir::Dob::Typesystem::ObjectPtr& object)
{
    auto oe = new DobObjectEditWidget(&m_dob, channelHandler, instance, object, this);
    connect(oe, &DobObjectEditWidget::XmlSerializedObject, this, &SateMainWindow::AddXmlPage);
    connect(oe, &DobObjectEditWidget::JsonSerializedObject, this, &SateMainWindow::AddJsonPage);
    AddTab(TypesystemRepository::Instance().GetClass(object->GetTypeId())->name, "OE", oe, m_midnightCommanderModeAction->isChecked());
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
    AddTab(title, "XML", textBrowser, false);
}

void SateMainWindow::AddJsonPage(const QString& title, const QString& text)
{
    QJsonDocument doc = QJsonDocument::fromJson(text.toUtf8());
    QString formattedJsonString = doc.toJson(QJsonDocument::Indented);
    auto textBrowser = new  QTextBrowser();
    textBrowser->setPlainText(formattedJsonString);
    AddTab(title,"JSON", textBrowser, false);
}

void SateMainWindow::AddTab(const QString& title,
                            const QString& tabType,
                            QWidget* widget,
                            const bool openInRightHandPanel)
{
    auto tabNameBeginning = tabType + title + " ";

    //find next number in series for the tab
    int lastNum = 0;
    for (auto* dock: m_dockManager->dockWidgetsMap())
    {
        const QString dockName = dock->objectName();
        if (dockName.startsWith(tabNameBeginning))
        {
            lastNum = std::max(lastNum, dockName.section(' ', -1).toInt());
        }
    }

    auto* dock = new ads::CDockWidget(tr("%1 %2").arg(title).arg(lastNum+1));
    dock->setObjectName(tr("%1%2 %3").arg(tabType).arg(title).arg(lastNum+1));
    widget->setParent(dock);
    dock->setWidget(widget, ads::CDockWidget::ForceNoScrollArea);
    dock->setFeature(ads::CDockWidget::DockWidgetDeleteOnClose, true);
    if (openInRightHandPanel)
    {
        if (m_rightDockArea == nullptr)
        {
            m_rightDockArea = m_dockManager->addDockWidget(ads::RightDockWidgetArea, dock, m_centralDockArea);
            connect(m_rightDockArea, &QObject::destroyed, this, [this]{m_rightDockArea = nullptr;});
        }
        else
        {
            m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_rightDockArea);
        }
    }
    else
    {
        m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
    }
}

namespace
{
    QString readStyleSheet(const QString& path)
    {
        QFile f(path);
        if (!f.exists())
        {
            throw std::logic_error(QString("Stylesheet %1 could not be found").arg(path).toStdString());
        }

        f.open(QFile::ReadOnly | QFile::Text);
        QTextStream ts(&f);
        return ts.readAll();
    }
}

void SateMainWindow::OnThemeChanged()
{
    QStringList mainStyleSheet;
    QStringList adsStyleSheet;

    if (ui->actionTouchMode->isChecked())
    {
        mainStyleSheet.append("* {font-size:20px;}");
    }

    if (ui->actionDarkMode->isChecked())
    {
        mainStyleSheet.append(readStyleSheet(":qdarkstyle/dark/darkstyle.qss"));
        mainStyleSheet.append(readStyleSheet(":customizations/tweaks-both.qss"));
        mainStyleSheet.append(readStyleSheet(":customizations/tweaks-dark.qss"));

        adsStyleSheet.append(readStyleSheet(":customizations/ads-dark.qss"));
    }
    else
    {
        mainStyleSheet.append(readStyleSheet(":qdarkstyle/light/lightstyle.qss"));
        mainStyleSheet.append(readStyleSheet(":customizations/tweaks-both.qss"));
        mainStyleSheet.append(readStyleSheet(":customizations/tweaks-light.qss"));

        adsStyleSheet.append(readStyleSheet(":customizations/ads-light.qss"));
    }

    qApp->setStyleSheet(mainStyleSheet.join("\n"));
    m_dockManager->setStyleSheet(adsStyleSheet.join("\n"));
}

void SateMainWindow::OnFocusedDockWidgetChanged(ads::CDockWidget* /*old*/, ads::CDockWidget* now)
{
    for (auto* widget: m_statusBarLabels)
    {
        ui->statusbar->removeWidget(widget);
        widget->deleteLater();
    }
    m_statusBarLabels.clear();
    for (auto* widget: m_statusBarSeparators)
    {
        ui->statusbar->removeWidget(widget);
        widget->deleteLater();
    }
    m_statusBarSeparators.clear();

    if (now == nullptr || now->widget() == nullptr)
    {
        return;
    }

    auto prop = now->widget()->property("statusBarInfo");
    if (prop.isValid())
    {
        auto* srcMo = now->widget()->metaObject();
        const auto srcMp = srcMo->property(srcMo->indexOfProperty("statusBarInfo"));
        const auto srcSignal = srcMp.notifySignal();
        //qDebug() << srcMo->className() << srcMp.name() << srcSignal.name();

        const auto dstMo = metaObject();
        const auto dstMethodIndex = dstMo->indexOfMethod("OnStatusBarInfoChanged()");
        const auto dstMethod = dstMo->method(dstMethodIndex);
        //qDebug() << dstMo->className() << dstMethodIndex << dstMethod.name();

        disconnect(m_currentStatusBarConnection);

        m_currentStatusBarConnection = connect(now->widget(),
                srcSignal,
                this,
                dstMethod);

        auto infos = prop.toStringList();
        for (auto it = infos.begin(); it != infos.end(); it+=2)
        {
            auto* label = new QLabel(*it);
            label->setToolTip(*(it+1));
            label->setMinimumWidth(100);
            m_statusBarLabels.push_back(label);
            ui->statusbar->addWidget(label);

            auto line = new QFrame;
            line->setObjectName("StatusBarSeparator");
            line->setFrameShape(QFrame::VLine);

            m_statusBarSeparators.push_back(line);
            ui->statusbar->addWidget(line);
        }
    }
}

void SateMainWindow::OnStatusBarInfoChanged()
{
    auto prop = m_dockManager->focusedDockWidget()->widget()->property("statusBarInfo");
    if (prop.isValid())
    {
        auto infos = prop.toStringList();
        for (auto it = infos.begin(); it != infos.end(); it+=2)
        {
            auto* label = m_statusBarLabels.at(std::distance(infos.begin(), it)/2);
            label->setText(*it);
            label->setToolTip(*(it+1));
        }
    }
}

void SateMainWindow::OnResetWindows()
{
    auto* focusedBefore = m_dockManager->focusedDockWidget();

    ads::CDockWidget* output = nullptr;
    ads::CDockWidget* typesystem = nullptr;

    for (auto* dock: m_dockManager->dockWidgetsMap())
    {
        const QString dockName = dock->objectName();
        if (dockName == "CentralWidget")
        {
            continue;
        }

        m_dockManager->removeDockWidget(dock);

        if (dockName == "Output")
        {
            output = dock;
        }
        else if (dockName == "Typesystem")
        {
            typesystem = dock;
        }
        else
        {
            m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
        }
    }
    //Need to add these last, to make sure they get back in the same place
    m_dockManager->addDockWidget(ads::LeftDockWidgetArea, typesystem);
    m_dockManager->addDockWidget(ads::BottomDockWidgetArea, output);

    if (focusedBefore != nullptr)
    {
        focusedBefore->raise();
    }

    m_midnightCommanderModeAction->setChecked(false);
}


void SateMainWindow::OnMidnightCommanderToggled()
{
    if (m_rightDockArea && !m_midnightCommanderModeAction->isChecked())
    {
        for (auto* dock: m_rightDockArea->dockWidgets())
        {
            m_dockManager->removeDockWidget(dock);
            m_dockManager->addDockWidget(ads::CenterDockWidgetArea, dock, m_centralDockArea);
        }
        m_rightDockArea->closeArea();
        m_rightDockArea = nullptr;
    }
}

void SateMainWindow::OnCloseCurrentTab()
{
    auto* focused = m_dockManager->focusedDockWidget();
    if (focused != nullptr)
    {
        focused->closeDockWidget();
    }
}

void SateMainWindow::OnCloseAllTabs()
{
    auto docks = m_dockManager->dockWidgetsMap();
        for (auto* dock: docks)
    {
        const QString dockName = dock->objectName();
        if (dockName == "CentralWidget" || dockName == "Output" || dockName == "Typesystem")
        {
            continue;
        }
        else
        {
            dock->deleteDockWidget();
        }
    }

}

void SateMainWindow::OnFindType()
{
    auto* focused = m_dockManager->focusedDockWidget();
    if (focused != nullptr)
    {
        const QString name = focused->tabWidget()->text().split(" ")[0];
        auto typeId = sdt::Operations::GetTypeId(name.toStdWString());
        m_typesystem->LocateType(typeId);
    }
}

void SateMainWindow::dragEnterEvent(QDragEnterEvent *event)
{
    if (event->mimeData()->hasUrls())
    {
        event->acceptProposedAction();
    }
}

void SateMainWindow::dropEvent(QDropEvent *e)
{
    if (e->mimeData()->hasUrls())
    {
        foreach (const QUrl &url, e->mimeData()->urls()) {
            QString fileName = url.toLocalFile();
            OpenSerializedObject(fileName);
        }
    }
}

void SateMainWindow::OpenSerializedObject(const QString& file)
{
    if (file.isNull())
    {
        return;
    }
    QFile f(file);
    if (f.exists())
    {
        f.open(QFile::ReadOnly | QFile::Text);
        QTextStream ts(&f);
        QString text = ts.readAll();
        auto probablyXml = file.endsWith(".xml", Qt::CaseInsensitive) ||
                           !( file.endsWith(".json", Qt::CaseInsensitive) || text.contains("_DouType"));

        try
        {
            auto objPtr = probablyXml ? sdt::Serialization::ToObject(text.toStdWString()) : sdt::Serialization::ToObjectFromJson(text.toStdWString());
            OnOpenObjectEditWithInstance({}, -1, objPtr);
        }
        catch (const std::exception& ex)
        {
            QString format = probablyXml ? "XML" : "JSON";
            QString error(ex.what());
            m_output->Output(QString("Failed to deserialize %1 object. %2").arg(format, error), QtFatalMsg);
        }
    }
    else
    {
        m_output->Output(QString("Deserialize object failed. File doesn't exist: %1").arg(file), QtFatalMsg);
    }
}
