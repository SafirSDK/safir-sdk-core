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
#include "settingsdialog.h"
#include "ui_settingsdialog.h"
#include "settings_manager.h"

#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QDialogButtonBox>
#include <QFileDialog>
#include <QMessageBox>

SettingsDialog::SettingsDialog(const Settings& states, const std::function<void(bool /*reset*/)>& editResonse, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::SettingsDialog)
    , m_editResonse(editResonse)
{
    ui->setupUi(this);
    setModal(true);

    setWindowTitle(tr("Settings"));

    ui->doDispatchCheckBox->setChecked(states.dispatch);
    ui->sendResponseCheckbox->setChecked(states.sendResponse);
    ui->createEntitiiesCheckbox->setChecked(states.createEntities);
    ui->updateEntitiesCheckbox->setChecked(states.updateEntities);
    ui->deleteEntitiesCheckbox->setChecked(states.deleteEntities);
    
    // Display current startup script path
    if (!states.startupScriptPath.isEmpty())
    {
        ui->currentStartupScriptLabel->setText(tr("Current: %1").arg(states.startupScriptPath));
    }
    else
    {
        ui->currentStartupScriptLabel->setText(tr("(none)"));
    }
    
    connect(ui->openResponsePushButton, &QPushButton::clicked, this, &SettingsDialog::onOpenResponseClicked);
    connect(ui->restoreResponsePushButton, &QPushButton::clicked, this, &SettingsDialog::onRestoreResponseClicked);
    connect(ui->selectStartupScriptPushButton, &QPushButton::clicked, this, &SettingsDialog::onSelectStartupScriptClicked);
    connect(ui->clearStartupScriptPushButton, &QPushButton::clicked, this, &SettingsDialog::onClearStartupScriptClicked);
}

SettingsDialog::~SettingsDialog()
{
    delete ui;
}

Settings SettingsDialog::getCheckboxStates() const
{
    Settings states;
    states.dispatch = ui->doDispatchCheckBox->isChecked();
    states.sendResponse = ui->sendResponseCheckbox->isChecked();
    states.createEntities = ui->createEntitiiesCheckbox->isChecked();
    states.updateEntities = ui->updateEntitiesCheckbox->isChecked();
    states.deleteEntities = ui->deleteEntitiesCheckbox->isChecked();
    return states;
}

void SettingsDialog::onOpenResponseClicked()
{
    if (m_editResonse)
    {
        m_editResonse(false);
    }
}

void SettingsDialog::onRestoreResponseClicked()
{
    if (m_editResonse)
    {
        m_editResonse(true);
    }
}

void SettingsDialog::onSelectStartupScriptClicked()
{
    QString fileName = QFileDialog::getOpenFileName(this,
        tr("Select Startup Script"),
        QString(),
        tr("Script Files (*.json);;All Files (*)"));
    
    if (!fileName.isEmpty())
    {
        SettingsManager settings;
        settings.saveStartupScript(fileName);
        ui->currentStartupScriptLabel->setText(tr("Current: %1").arg(fileName));
        QMessageBox::information(this, tr("Startup Script Set"),
            tr("Startup script set to: %1\n\nThis script will be executed automatically when SATE starts.").arg(fileName));
    }
}

void SettingsDialog::onClearStartupScriptClicked()
{
    SettingsManager settings;
    settings.saveStartupScript(QString());
    ui->currentStartupScriptLabel->setText(tr("(none)"));
    QMessageBox::information(this, tr("Startup Script Cleared"),
        tr("Startup script has been cleared. No script will be executed on startup."));
}
