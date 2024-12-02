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

#include <QWidget>
#include "typesystemrepository.h"
#include "dobhandler.h"
#include "typesysteminheritancemodel.h"
#include "typesystemnamespacemodel.h"
#include "typesystemcontextmenuhandler.h"

namespace Ui {
class TypesystemWidget;
}

class TypesystemWidget : public QWidget
{
    Q_OBJECT

public:
    explicit TypesystemWidget(QWidget *parent);
    ~TypesystemWidget();

    void Initialize(DobHandler* dob);

signals:
    void OpenObjectEdit(int64_t typeId);
    void OpenEntityInstanceViewer(int64_t typeId, bool includeSubclasses);
    void OpenMessageInstanceViewer(int64_t typeId,
                                   const Safir::Dob::Typesystem::ChannelId& channel,
                                   bool includeSubclasses);
    void OpenParameterViewer(int64_t typeId);
    void OpenDouFile(int64_t typeId);

private:
    Ui::TypesystemWidget *ui;
    DobHandler* m_dob = nullptr;
    TypesystemFilterProxyModel* m_inheritanceProxyModel = nullptr;
    TypesystemFilterProxyModel* m_namespaceProxyModel = nullptr;
    TypesystemContextMenuHandler* m_contextMenuHandler = nullptr;

    void ExpandTo(const QModelIndex& index);
    void SetTreeViewModel(bool inheritanceModel);
    void ApplyFilter(const QString& filterText);
};
