/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#endif

#include <QWidget>
#include <QLabel>
#include <QStringListModel>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include "ui_scriptwidget.h"
#include "scriptmodel.h"

class ScriptEngine;
class DobHandler;
class FileInfoWidget;

class ScriptWidget : public QWidget, public Ui::ScriptWidget
{
    Q_OBJECT

public:
    ScriptWidget(const QString& filePath, const QString& scriptText, DobHandler* dobHandler, QWidget *parent);
    ScriptWidget(const QString& filePath, const QString& scriptText, DobHandler* dobHandler, bool autoRun, QWidget *parent);

    ~ScriptWidget();

signals:
    // For internal use to know when to enable gui.
    void ScriptEngineLoaded();

private slots:
    void OnRunClicked();
    void OnPauseClicked();
    void OnResetClicked();
    void OnRecordClicked();
    void OnIndexFinished(int index);
    void OnScriptReset();
    void OnItemRecorded(const QJsonObject& item);
    void OnContextMenu(const QPoint& pos);
    void OnExecuteRow();
    void OnDeleteRow();
    void OnSaveClicked();
    void OnScriptEngineReady();

private:
    QString m_filePath;
    bool m_autoRun;
    std::unique_ptr<ScriptEngine> m_scriptEngine;
    ScriptModel* m_model;
    bool m_isModified = false;
    QStringListModel* m_errorModel = nullptr;
    FileInfoWidget* m_fileInfoWidget = nullptr;
    FileInfoWidget* m_errorFileInfoWidget = nullptr;

    void UpdateTitle();
    void ShowLoading(bool show);
    void InitializeUI();
    void ShowErrors(const QStringList& errors);
};
