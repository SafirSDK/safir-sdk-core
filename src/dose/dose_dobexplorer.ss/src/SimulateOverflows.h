/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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

#include "common_header.h"
#include "ui_SimulateOverflows.h"

#include <QTableWidgetItem>
#include <vector>

class QRegularExpression;

class SimulateOverflows :
  public QWidget,
  private Ui::SimulateOverflows
{
    Q_OBJECT

public:
    explicit SimulateOverflows(QWidget *parent = 0);

private slots:
    void onAddRow();
    void onPatternItemChanged(QTableWidgetItem* item);
    void onApply();
    void onClear();

private:
    enum Column
    {
        PatternColumn = 0,
        OutColumn     = 1,
        InColumn      = 2,
        MatchesColumn = 3,
        DeleteColumn  = 4
    };

    void addRow(const QString& pattern, bool outChecked, bool inChecked);
    void removeRow(int row);

    void validatePatternCell(int row);
    void updateApplyButtonState();
    void updateMatchesColumn();

    struct PatternInfo
    {
        QRegularExpression regex;
        bool outChecked;
        bool inChecked;
        int matchCount = 0;
        enum Status { Valid, Empty, Invalid } status;
    };
    std::vector<PatternInfo> collectPatterns() const;

    void setDirty(bool dirty);
    bool m_dirty;
};
