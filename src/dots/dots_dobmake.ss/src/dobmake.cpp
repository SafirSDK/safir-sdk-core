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
#include <QDesktopServices>
#include <QUrl>

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
    ui->archButtons->hide();
    ui->archLabel->hide();
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    ui->configRadioButtons->hide();
    m_debug = true;
    m_release = true;
    if (sizeof (void*) != 8)
    {
        ui->archButtons->hide();
        ui->archLabel->hide();
    }
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
    const QFileInfo fi(QFileDialog::getOpenFileName(this,
                                                    "Locate your dou-file directory",
                                                    "",
                                                    "CMakeLists.txt"));
    ui->douDirectory->setText(QDir::toNativeSeparators(fi.dir().path()));
}

void Dobmake::on_douDirectory_textChanged(const QString &path)
{
    const QFileInfo dir(path);
    const QFileInfo cmakelists(path + QDir::separator() + "CMakeLists.txt");

    if (dir.exists() && dir.isDir() && cmakelists.exists() && cmakelists.isFile())
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
    const QFileInfo dir(path);
    if (dir.exists() && dir.isDir())
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
        ui->installDirectory->setText(QDir::toNativeSeparators(dialog.selectedFiles()[0]));
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


void Dobmake::OpenLog(const bool ignoreCheckbox)
{
    if (ignoreCheckbox || ui->showLog->isChecked())
    {
        const QUrl url = QUrl::fromLocalFile(ui->douDirectory->text() + QDir::separator() + "buildlog.html");

        const bool result = QDesktopServices::openUrl(url);

        if (!result)
        {
            QMessageBox::warning(this,
                                 "Failed to open browser",
                                 "Failed to open the log in your web browser.\n"
                                 "Please open the buildlog.html file in the dou directory manually.");
        }
    }
}
void Dobmake::on_build_clicked()
{
    BuildThread* worker = new BuildThread(this,
                                          GetDobmakeBatchScript(),
                                          ui->douDirectory->text(),
                                          m_debug,
                                          m_release,
                                          ui->radio32bit->isChecked(),
                                          ""); //no installation

    connect(worker, SIGNAL(BuildComplete(bool)), this, SLOT(BuildComplete(bool)));
    connect(worker, SIGNAL(finished()), worker, SLOT(deleteLater()));
    m_buildRunning = true;
    QApplication::setOverrideCursor(Qt::BusyCursor);
    UpdateBuildButton();
    UpdateInstallButton();
    OpenLog();
    worker->start();
}

void Dobmake::on_buildAndInstall_clicked()
{
    BuildThread* worker = new BuildThread(this,
                                          GetDobmakeBatchScript(),
                                          ui->douDirectory->text(),
                                          m_debug,
                                          m_release,
                                          ui->radio32bit->isChecked(),
                                          ui->installDirectory->text());

    connect(worker, SIGNAL(BuildComplete(bool)), this, SLOT(BuildComplete(bool)));
    connect(worker, SIGNAL(finished()), worker, SLOT(deleteLater()));
    m_buildRunning = true;
    QApplication::setOverrideCursor(Qt::BusyCursor);
    UpdateBuildButton();
    UpdateInstallButton();
    OpenLog();
    worker->start();
}

void Dobmake::BuildComplete(const bool result)
{
    m_buildRunning = false;
    QApplication::restoreOverrideCursor();
    UpdateBuildButton();
    UpdateInstallButton();

    if (result)
    {
        QMessageBox::information(this,"Build successful!", "Build was completed successfully!");
    }
    else
    {
        QMessageBox box(QMessageBox::Critical,
                        "Build failed!",
                        "Build failed!\nPlease check your dou and CMakeLists.txt files for errors.",
                        QMessageBox::Ok);
        QAbstractButton* showLog = box.addButton("Show Log", QMessageBox::ApplyRole);
        box.exec();
        if (box.clickedButton() == showLog)
        {
            OpenLog(true);
        }
    }
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

void Dobmake::on_showLog_toggled(const bool checked)
{
    //This allows the user to check the box while the build is running
    //which will open the log.
    if (m_buildRunning && checked)
    {
        OpenLog();
    }
}
