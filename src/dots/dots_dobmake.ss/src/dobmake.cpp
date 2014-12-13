/******************************************************************************
*
* Copyright Consoden AB, 2014 (http://www.consoden.se)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include "dobmake.h"
#include <iostream>
#include "BuildThread.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#pragma warning (disable: 4251)
#endif

#include "ui_dobmake.h"
#include <QFileDialog>
#include <QProcess>
#include <QMessageBox>

#ifdef _MSC_VER
#pragma warning(pop)
#endif


Dobmake::Dobmake(QWidget *parent)
    : QDialog(parent)
    , m_buildRunning(false)
    , ui(new Ui::Dobmake)
{
    ui->setupUi(this);
    ui->douDirectory->setText("");
    ui->installDirectory->setText("");

#if defined(linux) || defined(__linux) || defined(__linux__)
    ui->configCheckButtons->hide();
    m_debug = false;
    m_release = true;
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    ui->configRadioButtons->hide();
    m_debug = true;
    m_release = true;
#else
#  error Dobmake does not know how to handle this platform
#endif
}

Dobmake::~Dobmake()
{
    delete ui;
}


bool Dobmake::CheckPython()
{
    QStringList params;
    params << "--version";

    QProcess p;
    p.start("python", params);
    p.waitForFinished(-1);

    return p.error() == QProcess::UnknownError && p.exitStatus() == QProcess::NormalExit && p.exitCode() == 0;
}

QString Dobmake::GetDobmakeBatchScript()
{
#if defined(linux) || defined(__linux) || defined(__linux__)
    const QString separator = ":";
    const QString scriptSuffix = "";
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    const QString separator = ";";
    const QString scriptSuffix = ".py";
#else
#  error Dobmake does not know how to handle this platform
#endif

    const QString pathenv = QProcessEnvironment::systemEnvironment().value("PATH");
    const QStringList paths = pathenv.split(separator,QString::SkipEmptyParts);

    for (QStringList::const_iterator it = paths.begin();
         it != paths.end(); ++it)
    {
        QFile f(*it + QDir::separator() + "dobmake-batch" + scriptSuffix);
        if (f.exists())
        {
            return f.fileName();
        }
    }
    return "";
}

void Dobmake::on_douDirectoryBrowse_clicked()
{
    QFileDialog dialog;
    dialog.setFileMode(QFileDialog::Directory);
    dialog.setOption(QFileDialog::ShowDirsOnly);

    if (dialog.exec())
    {
        ui->douDirectory->setText(dialog.selectedFiles()[0]);
    }
}

void Dobmake::on_douDirectory_textChanged(const QString &path)
{
    const QFile dir(path);
    const QFile cmakelists(path + QDir::separator() + "CMakeLists.txt");

    if (dir.exists() && cmakelists.exists())
    {
        ui->douDirectory->setStyleSheet("");
    }
    else
    {
        ui->douDirectory->setStyleSheet("Background-color:red");
    }

    UpdateBuildButton();
    UpdateInstallButton();
}

void Dobmake::on_installDirectory_textChanged(const QString &path)
{
    const QFile dir(path);
    if (dir.exists())
    {
        ui->installDirectory->setStyleSheet("");
    }
    else
    {
        ui->installDirectory->setStyleSheet("Background-color:red");
    }

    UpdateInstallButton();
}

void Dobmake::on_installDirectoryBrowse_clicked()
{
    QFileDialog dialog;
    dialog.setFileMode(QFileDialog::Directory);
    dialog.setOption(QFileDialog::ShowDirsOnly);

    if (dialog.exec())
    {
        ui->installDirectory->setText(dialog.selectedFiles()[0]);
    }
}

void Dobmake::UpdateInstallButton()
{
    const QFile buildDir(ui->douDirectory->text());
    const QFile cmakelists(ui->douDirectory->text() + QDir::separator() + "CMakeLists.txt");
    const QFile installDir(ui->installDirectory->text());
    ui->buildAndInstall->setEnabled(!m_buildRunning &&
                                    buildDir.exists() &&
                                    cmakelists.exists() &&
                                    installDir.exists());
}

void Dobmake::UpdateBuildButton()
{
    const QFile dir(ui->douDirectory->text());
    const QFile cmakelists(ui->douDirectory->text() + QDir::separator() + "CMakeLists.txt");
    ui->build->setEnabled(!m_buildRunning &&
                          dir.exists() &&
                          cmakelists.exists());
}

void Dobmake::on_build_clicked()
{
    BuildThread* worker = new BuildThread(this,
                                      GetDobmakeBatchScript(),
                                      ui->douDirectory->text(),
                                      m_debug,
                                      m_release,
                                      ""); //no installation

    connect(worker, SIGNAL(BuildComplete(bool)), this, SLOT(BuildComplete(bool)));
    connect(worker, SIGNAL(finished()), worker, SLOT(deleteLater()));
    m_buildRunning = true;
    UpdateBuildButton();
    UpdateInstallButton();
    worker->start();
}

void Dobmake::on_buildAndInstall_clicked()
{
    BuildThread* worker = new BuildThread(this,
                                          GetDobmakeBatchScript(),
                                          ui->douDirectory->text(),
                                          m_debug,
                                          m_release,
                                          ui->installDirectory->text());

    connect(worker, SIGNAL(BuildComplete(bool)), this, SLOT(BuildComplete(bool)));
    connect(worker, SIGNAL(finished()), worker, SLOT(deleteLater()));
    m_buildRunning = true;
    UpdateBuildButton();
    UpdateInstallButton();
    worker->start();
}

void Dobmake::BuildComplete(const bool result)
{
    m_buildRunning = false;
    UpdateBuildButton();
    UpdateInstallButton();

    if (result)
    {
        QMessageBox::information(this,"Build successful!", "Build was completed successfully!");
    }
    else
    {
        QMessageBox::critical(this,
                              "Build failed!",
                              "Build failed!\nPlease check your dou and CMakeLists.txt files for errors.");
    }
    //TODO: make the error box have one button saying "open log"
    //TODO: add support for the show log button.

}

void Dobmake::on_debugRadioButton_clicked(bool checked)
{
    m_debug = checked;
    m_release = !checked;
}

void Dobmake::on_releaseRadioButton_clicked(bool checked)
{
    m_debug = !checked;
    m_release = checked;
}

void Dobmake::on_debugCheckButton_clicked(bool checked)
{
    m_debug = checked;
}

void Dobmake::on_releaseCheckButton_clicked(bool checked)
{
    m_release = checked;
}
