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
#include "typesystemcontextmenuhandler.h"
#include "registerhandlerdialog.h"
#include <QPoint>
#include <QCursor>
#include <QMenu>
#include <QDebug>
#include <QMessageBox>

TypesystemContextMenuHandler::TypesystemContextMenuHandler(DobInterface* dob, QTreeView* parent)
    : QObject(parent)
    , m_dob(dob)
    , m_treeView(parent)
    , m_registerDlg(new RegisterHandlerDialog(parent))
{
    m_treeView->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(m_treeView, &QTreeView::customContextMenuRequested, [this](const QPoint&){
        auto ix = m_treeView->currentIndex();
        if (ix.isValid())
        {
            auto tid = ix.data(TypesystemRepository::DobTypeIdRole);
            auto bc = ix.data(TypesystemRepository::DobBaseClassRole);

            if (tid.isValid())
            {
                if (bc.isValid())
                {
                    // A class type
                    CreateContextMenu(tid.toLongLong(), static_cast<TypesystemRepository::DobBaseClass>(bc.toInt()));
                }
                else
                {
                    // Valid typeId but no baseClass, must be an Enum
                    QMenu menu;
                    int64_t typeId = tid.toLongLong();
                    connect(menu.addAction("Open dou-file"), &QAction::triggered, this, [this, typeId]{ emit OpenDouFile(typeId);});
                    menu.exec(m_treeView->cursor().pos());
                }
            }
        }
    });

    connect(m_registerDlg, &QDialog::accepted, this, [this]
    {
        auto typeId = m_registerDlg->TypeId();
        if (TypesystemRepository::Instance().GetClass(typeId)->dobBaseClass == TypesystemRepository::Entity)
        {
            m_dob->RegisterEntityHandler(typeId, m_registerDlg->Handler().toStdWString(), m_registerDlg->InstancePolicy(), m_registerDlg->Pending(), m_registerDlg->InjectionHandler());
        }
        else
        {
            m_dob->RegisterServiceHandler(typeId, m_registerDlg->Handler().toStdWString(), m_registerDlg->Pending());
        }
    } );
}

void TypesystemContextMenuHandler::CreateContextMenu(int64_t typeId, TypesystemRepository::DobBaseClass baseClass)
{
    QMenu menu;
    switch(baseClass) {
    case TypesystemRepository::Entity:
    {
        connect(menu.addAction("Register handler"), &QAction::triggered, this, [this, typeId]{ m_dob->RegisterEntityHandler(typeId, sdt::HandlerId(), Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId, false, false); });
        connect(menu.addAction("Register handler..."), &QAction::triggered, this, [this, typeId]{ m_registerDlg->Show(typeId); });
        connect(menu.addAction("Unregister handler"), &QAction::triggered, this, [this, typeId]{ m_dob->Unregister(typeId); });
#if 0
        connect(menu.addAction("Subscribe"), &QAction::triggered, this, [this, typeId]
        {
            m_dob->SubscribeRegistrations(typeId, sdt::HandlerId::ALL_HANDLERS, true);
            m_dob->SubscribeEntity(typeId, sdt::InstanceId(), true);
        });
        connect(menu.addAction("Subscribe..."), &QAction::triggered, this, []{QMessageBox m;m.setText("TODO: sub with options");m.exec();});
        connect(menu.addAction("Unsubscribe"), &QAction::triggered, this, [this, typeId]
        {
            m_dob->UnsubscribeRegistrations(typeId);
            m_dob->UnsubscribeEntity(typeId);
        });
        menu.addSeparator();
#endif
        connect(menu.addAction("Subscribe"), &QAction::triggered, this, [this, typeId]{ emit OpenInstanceViewer(typeId, false); });
        connect(menu.addAction("Subscribe recursive"), &QAction::triggered, this, [this, typeId]{ emit OpenInstanceViewer(typeId, true); });
    }
        break;
    case TypesystemRepository::Message:
    {
        connect(menu.addAction("Subscribe"), &QAction::triggered, this, [this, typeId]{ m_dob->SubscribeMessage(typeId, sdt::ChannelId(), true); });
        connect(menu.addAction("Subscribe..."), &QAction::triggered, this, []{QMessageBox m;m.setText("sub with options");m.exec();});
        connect(menu.addAction("Unsubscribe"), &QAction::triggered, this, [this, typeId]{m_dob->UnsubscribeMessage(typeId);});
        menu.addSeparator();
    }
        break;
    case TypesystemRepository::Service:
    {
        connect(menu.addAction("Register handler"), &QAction::triggered, this, [this, typeId]{ m_dob->RegisterServiceHandler(typeId, sdt::HandlerId(), false); });
        connect(menu.addAction("Register handler..."), &QAction::triggered, this, [this, typeId]{ m_registerDlg->Show(typeId, false); });
        connect(menu.addAction("Unregister handler"), &QAction::triggered, this, [this, typeId]{ m_dob->Unregister(typeId); });
        connect(menu.addAction("Subscribe registrations"), &QAction::triggered, this, [this, typeId]{m_dob->SubscribeRegistrations(typeId, sdt::HandlerId::ALL_HANDLERS, true);});
        connect(menu.addAction("Unsubscribe"), &QAction::triggered, this, [this, typeId]{m_dob->UnsubscribeRegistrations(typeId);});
        menu.addSeparator();
    }
        break;

    case TypesystemRepository::Response:
    case TypesystemRepository::Parametrization:
    case TypesystemRepository::Item:
    case TypesystemRepository::Struct:
    case TypesystemRepository::Object:
        break;
    }

    connect(menu.addAction("Open ObjectEditor"), &QAction::triggered, this, [this, typeId]{ emit OpenObjectEdit(typeId); });
    connect(menu.addAction("Open dou-file"), &QAction::triggered, this, [this, typeId]{ emit OpenDouFile(typeId);});

    menu.exec(m_treeView->cursor().pos());
}
