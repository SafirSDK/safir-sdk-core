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
#include <Safir/Dob/Typesystem/Object.h>
#include "dobinterface.h"

class DobObjectModel;

namespace Ui {
class DobObjectEditWidget;
}

class DobObjectEditWidget : public QWidget
{
    Q_OBJECT

public:
    explicit DobObjectEditWidget(DobInterface* dob, int64_t typeId, QWidget *parent);
    explicit DobObjectEditWidget(DobInterface* dob, int64_t typeId, QString channelHandler,
                                 int64_t instance, const Safir::Dob::Typesystem::ObjectPtr& object,  QWidget *parent);
    ~DobObjectEditWidget();

private:
    Ui::DobObjectEditWidget *ui;
    DobInterface* m_dob;
    int64_t m_typeId;

    void Init(DobObjectModel* model);
    void EditValue(const QModelIndex& index);

    bool eventFilter(QObject *object, QEvent *event) override;

    Safir::Dob::Typesystem::ObjectPtr BuildObject() const;
};
