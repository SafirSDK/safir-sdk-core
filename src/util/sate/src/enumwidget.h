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

namespace Ui {
class EnumWidget;
}

class EnumWidget : public QWidget
{
    Q_OBJECT

public:
    explicit EnumWidget(int64_t typeId, const QString& currentItem, QWidget *parent);
    ~EnumWidget();

private:
    Ui::EnumWidget *ui;

    void OnSectionResized(int index, int /*oldSize*/, int newSize);
    void ApplyFilter(const QString& filterText, int column, QWidget* filterWidget);
};
