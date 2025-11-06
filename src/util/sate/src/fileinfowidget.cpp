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
#include "fileinfowidget.h"
#include "ui_fileinfowidget.h"
#include <QFileInfo>
#include <QDir>
#include <QDesktopServices>
#include <QUrl>

FileInfoWidget::FileInfoWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::FileInfoWidget)
{
    ui->setupUi(this);
    ui->openContainingFolderButton->setVisible(false);
    connect(ui->openContainingFolderButton, &QToolButton::clicked,
            this, &FileInfoWidget::onOpenContainingFolderClicked);
}

FileInfoWidget::~FileInfoWidget()
{
    delete ui;
}

void FileInfoWidget::setInfo(const QString& info)
{
    ui->infoLabel->setText(info);
}

void FileInfoWidget::setFilePath(const QString& filePath)
{
    ui->filePathLabel->setText(filePath);
    
    // Enable/disable the button based on whether the file path exists
    QFileInfo fileInfo(filePath);
    if (fileInfo.exists())
    {
        ui->openContainingFolderButton->setVisible(true);
    }
    else
    {
        ui->openContainingFolderButton->setVisible(false);
    }
}

void FileInfoWidget::onOpenContainingFolderClicked()
{
    QString filePath = ui->filePathLabel->text();
    if (filePath.isEmpty())
    {
        return;
    }

    QFileInfo fileInfo(filePath);
    if (fileInfo.exists())
    {
        // Open the containing folder in the file manager
        QString folder = fileInfo.absolutePath();
        QDesktopServices::openUrl(QUrl::fromLocalFile(folder));
    }
}
