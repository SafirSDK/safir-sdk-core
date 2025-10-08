/******************************************************************************
*
* Copyright Saab AB, 2014-2015,2025 (http://safirsdkcore.com)
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
#include <boost/lexical_cast.hpp>

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
#include <QApplication>

#ifdef _MSC_VER
#pragma warning(pop)

#include <windows.h>
#endif

namespace
{
    bool Force32Bit()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
        return false;
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        BOOL isWow64;
        if (!IsWow64Process(GetCurrentProcess(),&isWow64))
        {
            throw std::runtime_error("Call to IsWow64Process(...) failed! Error Code = " +
                                     boost::lexical_cast<std::string>(GetLastError()));
        }
        return isWow64 ? true : false; //sic!
#else
#  error Dobmake does not know how to handle this platform
#endif

    }

    class ExposeSleep : public QThread
    {
    public:
        static inline void msleep(unsigned long msecs) {
            QThread::msleep(msecs);
        }
    };
}

Dobmake::Dobmake(QWidget *parent)
    : QDialog(parent)
    , m_buildRunning(false)
    , ui(new Ui::Dobmake)
    , m_settingsManager()
{
    ui->setupUi(this);
    ui->douDirectory->setText("");
    ui->installDirectory->setText("");
    resize(size().width(),minimumSize().height());
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

    //------------------------------------------------------------------
    // Restore persisted settings --------------------------------------
    //------------------------------------------------------------------
    const QString savedDou = m_settingsManager.loadDouDirectory();
    if (!savedDou.isEmpty()) {
        ui->douDirectory->setText(savedDou);
    }

    m_debug   = m_settingsManager.loadDebug();
    m_release = m_settingsManager.loadRelease();

#if defined(linux) || defined(__linux) || defined(__linux__)
    ui->debugRadioButton->setChecked(m_debug);
    ui->releaseRadioButton->setChecked(m_release);
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    ui->debugCheckButton->setChecked(m_debug);
    ui->releaseCheckButton->setChecked(m_release);
#endif

    ui->relativeInstall->setChecked(m_settingsManager.loadRelativeInstall());
    ui->absoluteInstall->setChecked(m_settingsManager.loadAbsoluteInstall());
    ui->showLog->setChecked(m_settingsManager.loadShowLog());
    ui->disableUnityBuild->setChecked(m_settingsManager.loadDisableUnityBuild());

    UpdateBuildButton();
    UpdateInstallButton();
    UpdateCleanButton();
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
    const QStringList paths = pathenv.split(separator);

    for (QStringList::const_iterator it = paths.begin();
         it != paths.end(); ++it)
    {
        if (it->isEmpty())
        {
            continue;
        }

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
    QString startDir;
    const QFileInfo dir(ui->douDirectory->text());
    if (dir.exists() && dir.isDir())
    {
        startDir = dir.path();
    }

    QString douDir = QFileDialog::getOpenFileName(this,
                                                  "Locate your dou-file directory",
                                                  startDir,
                                                  "CMakeLists.txt");
    if (!douDir.isNull())
    {
        const QFileInfo fi(douDir);
        ui->douDirectory->setText(QDir::toNativeSeparators(fi.dir().path()));
    }
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
    UpdateCleanButton();

    m_settingsManager.saveDouDirectory(path);
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
    const QFileInfo dir(ui->installDirectory->text());

    QFileDialog dialog;
    dialog.setFileMode(QFileDialog::Directory);
    dialog.setOption(QFileDialog::ShowDirsOnly);

    if (dir.exists() && dir.isDir())
    {
        dialog.setDirectory(dir.path());
    }
    else
    {
        dialog.setDirectory(QDir::home());
    }

    if (dialog.exec())
    {
        ui->installDirectory->setText(QDir::toNativeSeparators(dialog.selectedFiles()[0]));
    }
}

void Dobmake::on_relativeInstall_clicked()
{
    UpdateInstallButton();
    m_settingsManager.saveRelativeInstall(true);
    m_settingsManager.saveAbsoluteInstall(false);
}

void Dobmake::on_absoluteInstall_clicked()
{
    UpdateInstallButton();
    m_settingsManager.saveRelativeInstall(false);
    m_settingsManager.saveAbsoluteInstall(true);
}

void Dobmake::UpdateInstallButton()
{
    const QFile buildDir(ui->douDirectory->text());
    const QFile cmakelists(ui->douDirectory->text() + QDir::separator() + "CMakeLists.txt");
    const QFile installDir(ui->installDirectory->text());
    ui->buildAndInstall->setEnabled(!m_buildRunning &&
                                    buildDir.exists() &&
                                    cmakelists.exists() &&
                                    (installDir.exists() || ui->absoluteInstall->isChecked()));
}

void Dobmake::UpdateBuildButton()
{
    const QFile dir(ui->douDirectory->text());
    const QFile cmakelists(ui->douDirectory->text() + QDir::separator() + "CMakeLists.txt");
    ui->build->setEnabled(!m_buildRunning &&
                          dir.exists() &&
                          cmakelists.exists());
}

void Dobmake::UpdateCleanButton()
{
    const QFile dir(ui->douDirectory->text());
    const QFile cmakelists(ui->douDirectory->text() + QDir::separator() + "CMakeLists.txt");
    ui->clean->setEnabled(!m_buildRunning &&
                          dir.exists() &&
                          cmakelists.exists());
}


void Dobmake::OpenLog(const bool ignoreCheckbox)
{
    if (ignoreCheckbox || ui->showLog->isChecked())
    {
        const QUrl url = QUrl::fromLocalFile(ui->douDirectory->text() + QDir::separator() + "buildlog.html");

        //try to open for thirty seconds, once every 0.1s
        for (int i = 0; i < 300; ++i)
        {
            const bool result = QDesktopServices::openUrl(url);
            if (result)
            {
                return;
            }
            ExposeSleep::msleep(100);
        }

        QMessageBox::warning(this,
                             "Failed to open browser",
                             "Failed to open the log in your web browser.\n"
                             "Please open the buildlog.html file in the dou directory manually.");
    }
}
void Dobmake::on_build_clicked()
{
    BuildThread* worker = new BuildThread(this,
                                          GetDobmakeBatchScript(),
                                          ui->douDirectory->text(),
                                          m_debug,
                                          m_release,
                                          Force32Bit(),
                                          "", //no installation
                                          false, //not cleaning
                                          ui->disableUnityBuild->isChecked());
    connect(worker, SIGNAL(BuildComplete(bool)), this, SLOT(BuildComplete(bool)));
    connect(worker, SIGNAL(finished()), worker, SLOT(deleteLater()));
    m_buildRunning = true;
    QApplication::setOverrideCursor(Qt::BusyCursor);
    UpdateBuildButton();
    UpdateInstallButton();
    UpdateCleanButton();
    worker->start();
    OpenLog();
}

void Dobmake::on_buildAndInstall_clicked()
{
    QString installDir;
    if (ui->relativeInstall->isChecked())
    {
        installDir = ui->installDirectory->text();
    }
    else
    {
        installDir = "None";
    }

    BuildThread* worker = new BuildThread(this,
                                          GetDobmakeBatchScript(),
                                          ui->douDirectory->text(),
                                          m_debug,
                                          m_release,
                                          Force32Bit(),
                                          installDir,
                                          false,
                                          ui->disableUnityBuild->isChecked());

    connect(worker, SIGNAL(BuildComplete(bool)), this, SLOT(BuildComplete(bool)));
    connect(worker, SIGNAL(finished()), worker, SLOT(deleteLater()));
    m_buildRunning = true;
    QApplication::setOverrideCursor(Qt::BusyCursor);
    UpdateBuildButton();
    UpdateInstallButton();
    UpdateCleanButton();
    worker->start();
    OpenLog();
}

void Dobmake::on_clean_clicked()
{
    BuildThread* worker = new BuildThread(this,
                                          GetDobmakeBatchScript(),
                                          ui->douDirectory->text(),
                                          m_debug,
                                          m_release,
                                          Force32Bit(),
                                          "", //no installation
                                          true, //cleaning
                                          ui->disableUnityBuild->isChecked());


    connect(worker, SIGNAL(BuildComplete(bool)), this, SLOT(BuildComplete(bool)));
    connect(worker, SIGNAL(finished()), worker, SLOT(deleteLater()));
    m_buildRunning = true;
    QApplication::setOverrideCursor(Qt::BusyCursor);
    UpdateBuildButton();
    UpdateInstallButton();
    UpdateCleanButton();
    worker->start();
    OpenLog();
}


void Dobmake::BuildComplete(const bool result)
{
    m_buildRunning = false;
    QApplication::restoreOverrideCursor();
    UpdateBuildButton();
    UpdateInstallButton();
    UpdateCleanButton();

    if (result)
    {
        QMessageBox::information(this,"Operation successful!", "Operation was completed successfully!");
    }
    else
    {
        QMessageBox box(QMessageBox::Critical,
                        "Operation failed!",
                        "Operation failed!\nPlease check your dou and CMakeLists.txt files for errors.",
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

    m_settingsManager.saveDebug(m_debug);
    m_settingsManager.saveRelease(m_release);
}

void Dobmake::on_releaseRadioButton_clicked(bool checked)
{
    m_debug = !checked;
    m_release = checked;

    m_settingsManager.saveDebug(m_debug);
    m_settingsManager.saveRelease(m_release);
}

void Dobmake::on_debugCheckButton_clicked(bool checked)
{
    m_debug = checked;
    m_settingsManager.saveDebug(m_debug);
}

void Dobmake::on_releaseCheckButton_clicked(bool checked)
{
    m_release = checked;
    m_settingsManager.saveRelease(m_release);
}

void Dobmake::on_showLog_toggled(const bool checked)
{
    m_settingsManager.saveShowLog(checked);

    //This allows the user to check the box while the build is running
    //which will open the log.
    if (m_buildRunning && checked)
    {
        OpenLog();
    }
}

void Dobmake::on_disableUnityBuild_toggled(const bool checked)
{
    m_settingsManager.saveDisableUnityBuild(checked);
}

void Dobmake::on_clearSettingsButton_clicked()
{
    const QMessageBox::StandardButton reply = QMessageBox::question(
        this,
        tr("Clear settings"),
        tr("Are you sure you want to clear all settings?\nThe application will exit."),
        QMessageBox::Yes | QMessageBox::No,
        QMessageBox::No);

    if (reply == QMessageBox::Yes)
    {
        m_settingsManager.clearAll();
        qApp->quit();
    }
}
