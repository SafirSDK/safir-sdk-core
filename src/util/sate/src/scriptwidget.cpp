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
#include "scriptwidget.h"
#include "scriptengine.h"
#include "scriptmodel.h"
#include "fileinfowidget.h"
#include <QMessageBox>
#include <QMenu>
#include <QFileDialog>
#include <QApplication>
#include <QMainWindow>
#include <QStatusBar>
#include <QLabel>
#include <QtConcurrent/QtConcurrent>
#include <QStringListModel>

ScriptWidget::ScriptWidget(const QString& filePath, const QString& scriptText, DobHandler* dobHandler, QWidget *parent)
    : ScriptWidget(filePath, scriptText, dobHandler, false, parent)
{
}

ScriptWidget::ScriptWidget(const QString& filePath, const QString& scriptText, DobHandler* dobHandler, bool autoRun, QWidget *parent)
    : QWidget(parent)
    , m_filePath(filePath)
    , m_autoRun(autoRun)
    , m_scriptEngine(nullptr)
    , m_model(nullptr)
    , m_isModified(false)
    , m_errorModel(nullptr)
{
    setupUi(this);

    // Create and setup FileInfoWidget
    m_fileInfoWidget = new FileInfoWidget(this);
    fileInfoLayout->addWidget(m_fileInfoWidget);

    if (m_filePath.isEmpty())
    {
        m_fileInfoWidget->setInfo("New Script");
        m_fileInfoWidget->setFilePath("");
        loadingLabel->setText("Loading script...");
    }
    else
    {
        m_fileInfoWidget->setInfo("Script");
        m_fileInfoWidget->setFilePath(m_filePath);
        loadingLabel->setText(QString("Loading script: %1").arg(m_filePath));
    }

    // Show loading page
    stackedWidget->setCurrentWidget(loadingPage);

    // Disable buttons while loading
    recordButton->setDisabled(true);
    runScriptButton->setDisabled(true);
    pauseButton->setDisabled(true);
    resetButton->setDisabled(true);
    saveButton->setDisabled(true);
    scriptTableView->setDisabled(true);

    m_scriptEngine = std::make_unique<ScriptEngine>(dobHandler);

    // Load script in separate thread
    connect(this, &ScriptWidget::ScriptEngineLoaded, this, &ScriptWidget::OnScriptEngineReady, Qt::QueuedConnection);
    auto dummy = QtConcurrent::run([this, scriptText]{
        m_scriptEngine->LoadScript(scriptText);
        emit ScriptEngineLoaded();
    });
}

ScriptWidget::~ScriptWidget()
{
    if (m_scriptEngine)
    {
        // If we were recording, this will disable recording in the DobHandler
        m_scriptEngine->Reset();
    }
}

void ScriptWidget::UpdateTitle()
{
    if (!m_fileInfoWidget)
        return;

    QString info;
    if (m_filePath.isEmpty())
    {
        info = QString("New Script");
    }
    else
    {
        info = QString("Script");
    }

    if (m_isModified)
    {
        info += " *";
    }

    m_fileInfoWidget->setInfo(info);
    m_fileInfoWidget->setFilePath(m_filePath);
}

void ScriptWidget::OnRecordClicked()
{
    if (m_scriptEngine)
    {
        if (m_scriptEngine->CurrentState() == ScriptEngine::Recording)
        {
            OnResetClicked(); // Stop recording resets the script engine
        }
        else
        {
            // Start recording
            m_scriptEngine->Record();
            recordButton->setStyleSheet("color: #FAB900; font-weight: bold;");
            recordButton->setText("Stop Recording");
            runScriptButton->setStyleSheet("");
            pauseButton->setStyleSheet("");

            recordButton->setDisabled(false);
            runScriptButton->setDisabled(true);
            pauseButton->setDisabled(true);
            resetButton->setDisabled(true);
        }
    }
}

void ScriptWidget::OnRunClicked()
{
    if (m_scriptEngine)
    {
        m_scriptEngine->Execute();
        recordButton->setStyleSheet("");
        runScriptButton->setStyleSheet("color: #FAB900; font-weight: bold;");
        pauseButton->setStyleSheet("");

        runScriptButton->setDisabled(true);
        recordButton->setDisabled(true);
        pauseButton->setDisabled(false);
        resetButton->setDisabled(false);
    }
}

void ScriptWidget::OnPauseClicked()
{
    if (m_scriptEngine)
    {
        m_scriptEngine->Pause();
        recordButton->setStyleSheet("");
        runScriptButton->setStyleSheet("");
        pauseButton->setStyleSheet("color: #FAB900; font-weight: bold;");

        recordButton->setDisabled(true);
        runScriptButton->setDisabled(false);
        pauseButton->setDisabled(true);
        resetButton->setDisabled(false);
    }
}

void ScriptWidget::OnResetClicked()
{
    if (m_scriptEngine)
    {
        m_scriptEngine->Reset();
        recordButton->setStyleSheet("");
        runScriptButton->setStyleSheet("");
        pauseButton->setStyleSheet("");

        recordButton->setDisabled(false);
        runScriptButton->setDisabled(false);
        pauseButton->setDisabled(false);
        resetButton->setDisabled(false);

        recordButton->setText("Record");

        OnScriptReset();
    }
}

void ScriptWidget::OnIndexFinished(int index)
{
    if (m_model)
    {
        m_model->setRowRun(index, true);
        scriptTableView->scrollTo(m_model->index(index, 0)); // scroll to the located node
    }
}

void ScriptWidget::OnScriptReset()
{
    if (m_model)
    {
        for (int i = 0; i < m_model->rowCount(); ++i)
        {
            m_model->setRowRun(i, false);
        }
    }
}

void ScriptWidget::OnItemRecorded(const QJsonObject& item)
{
    if (m_model)
    {
        ScriptModel::ScriptRow row;
        row.method = item["method"].toString();
        row.details = QString::fromUtf8(QJsonDocument(item["params"].toObject()).toJson(QJsonDocument::Compact));
        row.run = false;
        m_model->addRow(row);

        if (!m_isModified)
        {
            m_isModified = true;
            UpdateTitle();
        }
    }
}

void ScriptWidget::OnContextMenu(const QPoint& pos)
{
    QModelIndex index = scriptTableView->indexAt(pos);
    if (!index.isValid())
        return;

    QMenu contextMenu(tr("Context menu"), this);
    QAction* executeAction = contextMenu.addAction(tr("Execute Row"));
    QAction* deleteAction = contextMenu.addAction(tr("Delete Row"));

    connect(executeAction, &QAction::triggered, this, &ScriptWidget::OnExecuteRow);
    connect(deleteAction, &QAction::triggered, this, &ScriptWidget::OnDeleteRow);

    contextMenu.exec(scriptTableView->viewport()->mapToGlobal(pos));
}

void ScriptWidget::OnExecuteRow()
{
    QModelIndex index = scriptTableView->currentIndex();
    if (!index.isValid())
        return;

    int row = index.row();
    if (m_scriptEngine && row >= 0 && row < m_scriptEngine->Size())
    {
        m_scriptEngine->ExecuteIndex(row);
    }
}

void ScriptWidget::OnDeleteRow()
{
    QModelIndex index = scriptTableView->currentIndex();
    if (!index.isValid())
        return;

    int row = index.row();
    if (m_model && row >= 0 && row < m_model->rowCount())
    {
        m_model->deleteRow(row);
        if (m_scriptEngine && row >= 0 && row < m_scriptEngine->Size())
        {
            m_scriptEngine->DeleteIndex(row);
        }

        if (!m_isModified)
        {
            m_isModified = true;
            UpdateTitle();
        }
    }
}

void ScriptWidget::OnSaveClicked()
{
    if (!m_scriptEngine)
        return;

    QString fileName = m_filePath;
    if (fileName.isEmpty())
    {
        fileName = QFileDialog::getSaveFileName(
            this,
            tr("Save Script"),
            QString(),
            tr("JSON Files (*.json);;All Files (*)")
        );
    }
    else
    {
        fileName = QFileDialog::getSaveFileName(
            this,
            tr("Save Script"),
            m_filePath,
            tr("JSON Files (*.json);;All Files (*)")
        );
    }

    if (fileName.isEmpty())
        return;

    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        QMessageBox::critical(this, tr("Save Error"),
            tr("Could not open file for writing: %1").arg(file.errorString()));
        return;
    }

    QString scriptText = m_scriptEngine->GetScript();
    QTextStream out(&file);
    out << scriptText;
    file.close();

    m_filePath = fileName;
    m_isModified = false;
    UpdateTitle();
}

void ScriptWidget::OnScriptEngineReady()
{
    disconnect(this, &ScriptWidget::ScriptEngineLoaded, this, &ScriptWidget::OnScriptEngineReady);
    if (m_scriptEngine->IsValid() == false)
    {
        ShowErrors(m_scriptEngine->Errors());
        return;
    }

    InitializeUI();
}

void ScriptWidget::ShowLoading(bool show)
{
    if (show)
    {
        stackedWidget->setCurrentWidget(loadingPage);
    }
}

void ScriptWidget::InitializeUI()
{
    scriptTableView->setSortingEnabled(false);
    scriptTableView->setSelectionBehavior(QTableView::SelectRows);
    scriptTableView->setSelectionMode(QAbstractItemView::SingleSelection);
    scriptTableView->verticalHeader()->setVisible(false);
    scriptTableView->setContextMenuPolicy(Qt::CustomContextMenu);

    // Build vector of ScriptRow from script engine
    QVector<ScriptModel::ScriptRow> rows;
    for (int i = 0; i < m_scriptEngine->Size(); ++i)
    {
        auto obj = m_scriptEngine->GetIndexObject(i);
        ScriptModel::ScriptRow row;
        row.method = obj["method"].toString();
        row.details = QString::fromUtf8(QJsonDocument(obj["params"].toObject()).toJson(QJsonDocument::Compact));
        row.run = false;
        rows.append(row);
    }

    m_model = new ScriptModel(rows, this);
    scriptTableView->setModel(m_model);

    // Set row number column width to fit number of rows.
    QFontMetrics fm(scriptTableView->font());
    int width = fm.horizontalAdvance(QString::number(m_scriptEngine->Size())) + 20;
    scriptTableView->setColumnWidth(ScriptModel::RowNumber, width);
    scriptTableView->setColumnWidth(ScriptModel::Method, 250);

    // Set Run column to minimal width
    int runWidth = fm.horizontalAdvance(tr("Run")) + 30;
    scriptTableView->setColumnWidth(ScriptModel::Run, runWidth);

    // Make Details column take remaining space
    scriptTableView->horizontalHeader()->setStretchLastSection(false);
    scriptTableView->horizontalHeader()->setSectionResizeMode(ScriptModel::Details, QHeaderView::Stretch);

    // Connect button signals
    connect(recordButton, &QPushButton::clicked, this, &ScriptWidget::OnRecordClicked);
    connect(runScriptButton, &QPushButton::clicked, this, &ScriptWidget::OnRunClicked);
    connect(pauseButton, &QPushButton::clicked, this, &ScriptWidget::OnPauseClicked);
    connect(resetButton, &QPushButton::clicked, this, &ScriptWidget::OnResetClicked);
    connect(saveButton, &QPushButton::clicked, this, &ScriptWidget::OnSaveClicked);

    // Connect script engine signals
    connect(m_scriptEngine.get(), &ScriptEngine::IndexFinished, this, &ScriptWidget::OnIndexFinished);
    connect(m_scriptEngine.get(), &ScriptEngine::ItemRecorded, this, &ScriptWidget::OnItemRecorded);

    // Connect context menu
    connect(scriptTableView, &QTableView::customContextMenuRequested, this, &ScriptWidget::OnContextMenu);

    // Enable widgets
    recordButton->setDisabled(false);
    runScriptButton->setDisabled(false);
    pauseButton->setDisabled(false);
    resetButton->setDisabled(false);
    saveButton->setDisabled(false);
    scriptTableView->setDisabled(false);

    // Show script page
    stackedWidget->setCurrentWidget(scriptPage);

    OnScriptReset();
    if (m_autoRun)
    {
        OnRunClicked();
    }
}

void ScriptWidget::ShowErrors(const QStringList& errors)
{
    if (!m_errorModel)
    {
        m_errorModel = new QStringListModel(this);
        errorListView->setModel(m_errorModel);
    }

    if (!m_errorFileInfoWidget)
    {
        m_errorFileInfoWidget = new FileInfoWidget(this);
        errorFileInfoLayout->addWidget(m_errorFileInfoWidget);
    }

    m_errorModel->setStringList(errors);

    // Update error file info widget
    m_errorFileInfoWidget->setInfo("Script Errors");
    m_errorFileInfoWidget->setFilePath(m_filePath);

    stackedWidget->setCurrentWidget(errorPage);
}
