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
#pragma once

#include <QTreeView>
#include "typesystemrepository.h"
#include "dobhandler.h"
#include "registerhandlerdialog.h"

class TypesystemContextMenuHandler : public QObject
{
    Q_OBJECT
public:
    TypesystemContextMenuHandler(DobHandler* dob, QTreeView* parent);

signals:
    void OpenObjectEdit(int64_t typeId);
    void OpenEntityInstanceViewer(int64_t typeId, bool includeSubclasses);
    void OpenMessageInstanceViewer(int64_t typeId,
                                   const Safir::Dob::Typesystem::ChannelId& channel,
                                   bool includeSubclasses);
    void OpenDouFile(int64_t typeId);

private:
    DobHandler* m_dob;
    QTreeView* m_treeView;
    RegisterHandlerDialog* m_registerDlg;

    void CreateContextMenu(int64_t typeId, TypesystemRepository::DobBaseClass baseClass);
};
