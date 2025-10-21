/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström
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
#include <vector>
#include <memory>
#include "highlight_rule.h"
#include "settings_manager.h"

class QTableWidget;
class QPushButton;

/* Table-based editor for highlight rules */
class HighlightWidget : public QWidget
{
    Q_OBJECT
public:
    explicit HighlightWidget(const std::shared_ptr<SettingsManager>& settingsManager, QWidget* parent = nullptr);
    std::vector<HighlightRule> rules() const;

signals:
    void rulesChanged();

private slots:
    void OnAddRow();
    void OnRemoveSelected();
    void OnCellChanged(int, int);
    void OnColorCellClicked(int, int);
    void OnMoveUp();
    void OnMoveDown();
    void OnSelectionChanged();

private:
    void EmitRulesChanged();
    void UpdateColorCell(int row);

    QTableWidget* m_table        = nullptr;
    QPushButton*  m_addButton    = nullptr;
    QPushButton*  m_removeButton = nullptr;
    QPushButton*  m_moveUpButton = nullptr;
    QPushButton*  m_moveDownButton = nullptr;
    std::shared_ptr<SettingsManager> m_settingsManager;
};
