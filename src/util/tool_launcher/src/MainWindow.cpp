/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
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
#include <string>

#include <iostream>

#include "MainWindow.h"

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4251)
#  pragma warning (disable: 4800)
#  pragma warning (disable: 4459)
#  pragma warning (disable: 4018)
#  include <boost/process/windows.hpp>
#endif

#include "ui_MainWindow.h"
#include <QPushButton>
#include <QTimer>
#include <QShortcut>
#include <QScrollBar>
#include <QDebug>

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

namespace
{
    QString concatenateCommand(const std::string& program, const std::vector<std::string>& args)
    {
        QString result = QString::fromStdString(program);
        for (const auto& arg: args)
        {
            result += " ";
            result += QString::fromStdString(arg);
        }
        return result;
    }
}

MainWindow::Process::~Process()
{
    AppendMetaText(textEdit, exitText, exitError);
}


MainWindow::MainWindow()
    : QMainWindow(nullptr)
    , m_ui(std::make_unique<Ui::MainWindow>())
    , m_work(std::make_unique<boost::asio::io_service::work>(m_ioService))
{
    m_ui->setupUi(this);
    //Note that a stylesheet is set on the dialog in qt designer.

    m_ui->outputTabs->setVisible(false);
    m_ui->lookupEdit->setPlaceholderText(tr("TypeId or Type name"));
    adjustSize();
    auto* shortcut = new QShortcut(QKeySequence::Cancel, this);
    connect(shortcut, &QShortcut::activated, this, &QMainWindow::close);

    connect(m_ui->launchSateButton, &QPushButton::pressed, this, &MainWindow::OnLaunchSatePressed);
    connect(m_ui->launchDobExplorerButton, &QPushButton::pressed, this, &MainWindow::OnLaunchDobExplorerPressed);
    connect(m_ui->launchSateLegacyButton, &QPushButton::pressed, this, &MainWindow::OnLaunchSateLegacyPressed);
    connect(m_ui->launchControlGuiButton, &QPushButton::pressed, this, &MainWindow::OnLaunchControlGuiPressed);
    connect(m_ui->checkDouFilesButton, &QPushButton::pressed, this, &MainWindow::OnCheckDouFilesPressed);
    connect(m_ui->checkGeneratedButton, &QPushButton::pressed, this, &MainWindow::OnCheckGeneratedPressed);
    connect(m_ui->showTypesystemDetailsButton, &QPushButton::pressed, this, &MainWindow::OnShowTypesystemDetailsPressed);
    connect(m_ui->lookupEdit, &QLineEdit::textChanged, this, [this]{
                      m_ui->typeIdLookupButton->setEnabled(!m_ui->lookupEdit->text().isEmpty());
                      m_ui->typeLookupButton->setEnabled(!m_ui->lookupEdit->text().isEmpty());
                  });
    connect(m_ui->typeIdLookupButton, &QPushButton::pressed, this, &MainWindow::OnTypeIdLookupPressed);
    connect(m_ui->typeLookupButton, &QPushButton::pressed, this, &MainWindow::OnTypeLookupPressed);
    connect(m_ui->showOutputButton, &QAbstractButton::toggled, this, &MainWindow::OnShowOutputToggled);
    connect(m_ui->allocateButton, &QPushButton::pressed, this, &MainWindow::OnAllocatePressed);
    connect(m_ui->deallocateButton, &QPushButton::pressed, this, &MainWindow::OnDeallocatePressed);
    connect(m_ui->outputTabs, &QTabWidget::tabCloseRequested, this, &MainWindow::OnCloseTab);

    auto* timer = new QTimer(this);
    timer->setInterval(100);
    timer->callOnTimeout([this]{m_ioService.poll(); });
    timer->start();
}

MainWindow::~MainWindow()
{
    for (auto& process: m_processes)
    {
        process->proc->detach();
    }
}


void MainWindow::LaunchProgram(const std::string& program, const std::vector<std::string>& args)
{
    std::error_code error;

    auto process = std::make_shared<Process>(m_ioService);

#ifdef _MSC_VER
    auto environment = boost::this_process::wenvironment();
    environment[L"SAFIR_INSTANCE"] = std::to_wstring(m_ui->safirInstanceSpin->value());
#else
    auto environment = boost::this_process::environment();
    environment["SAFIR_INSTANCE"] = std::to_string(m_ui->safirInstanceSpin->value());
#endif

    process->proc = std::make_unique<boost::process::child>
        (boost::process::search_path(program),
         boost::process::args(args),
         m_ioService,
         error,
         boost::process::on_exit=[this,process](int exitCode, const std::error_code& error){Exited(process, error, exitCode);},
         boost::process::std_out > process->outPipe,
         boost::process::std_err > process->errPipe,
#ifdef _MSC_VER
         boost::process::windows::create_no_window, //Avoid creating terminal window on Windows
#endif
         environment
         );

    process->textEdit = new QTextEdit(this);
    process->textEdit->setReadOnly(true);

    const int index = m_ui->outputTabs->
        addTab(process->textEdit, QString::fromStdString(program) + " " + QString::number(process->proc->id()));
    m_ui->outputTabs->setCurrentIndex(index);
    AppendMetaText(process->textEdit,
                   tr("Output from '%1' with pid %2 and SAFIR_INSTANCE=%3:\n")
                   .arg(concatenateCommand(program,args))
                   .arg(process->proc->id())
                   .arg(m_ui->safirInstanceSpin->value()),
                   false);

    if (error)
    {
        AppendMetaText(process->textEdit, tr("Failed to launch process: %1\n").arg(QString::fromStdString(error.message())), true);
    }
    else
    {
        PipeReadOutLoop(process);
        PipeReadErrLoop(process);
    }

    m_processes.insert(process);
}

void MainWindow::PipeReadOutLoop(const std::shared_ptr<Process>& process)
{
    process->outPipe.async_read_some
        (boost::asio::buffer(process->outBuf),
         [this,process](const boost::system::error_code& error, std::size_t size)
         {
             if (error == boost::asio::error::eof || error == boost::asio::error::broken_pipe)
             {
                 return;
             }
             else if (error)
             {
                 AppendMetaText(process->textEdit, tr("Failed to read process stdout: %1 (Error %2 %3:%4)\n").
                                arg(QString::fromStdString(error.message())).
                                arg(error.value()).
                                arg(QString::fromStdString(error.category().name())).
                                arg(QString::fromStdString(error.default_error_condition().message())),
                                true);
                 return;
             }

             AppendText(process->textEdit, QString::fromUtf8(process->outBuf.data(), static_cast<int>(size)));

             PipeReadOutLoop(process);
         });
}

void MainWindow::PipeReadErrLoop(const std::shared_ptr<Process>& process)
{
    process->errPipe.async_read_some
        (boost::asio::buffer(process->errBuf),
         [this,process](const boost::system::error_code& error, std::size_t size)
         {
             if (error == boost::asio::error::eof || error == boost::asio::error::broken_pipe)
             {
                 return;
             }
             else if (error)
             {
                 AppendMetaText(process->textEdit, tr("Failed to read process stderr: %1 (Error %2 %3:%4)\n").
                                arg(QString::fromStdString(error.message())).
                                arg(error.value()).
                                arg(QString::fromStdString(error.category().name())).
                                arg(QString::fromStdString(error.default_error_condition().message())),
                                true);
                 return;
             }

             AppendMetaText(process->textEdit,QString::fromUtf8(process->errBuf.data(), static_cast<int>(size)), true);

             PipeReadErrLoop(process);
      });
}

void MainWindow::Exited(const std::shared_ptr<Process>& process,
                        const std::error_code& error,
                        const int exitCode)
{
    if (error)
    {
        process->exitText = tr("Error in waiting for process to exit: %1\n").arg(QString::fromStdString(error.message()));
        process->exitError = true;
    }
    else
    {
        process->exitText = tr("Process exited with code %1\n").arg(exitCode);
        process->exitError = exitCode != 0;
    }

    m_processes.erase(process);
}


void MainWindow::OnLaunchSatePressed()
{
    LaunchProgram("sate");
}

void MainWindow::OnLaunchSateLegacyPressed()
{
    LaunchProgram("sate_legacy");
}

void MainWindow::OnLaunchDobExplorerPressed()
{
    LaunchProgram("dobexplorer");
}

void MainWindow::OnLaunchControlGuiPressed()
{
    LaunchProgram("safir_control_gui");
}

void MainWindow::OnCheckDouFilesPressed()
{
    m_ui->showOutputButton->setChecked(true);
    LaunchProgram("dots_configuration_check");
}

void MainWindow::OnCheckGeneratedPressed()
{
    m_ui->showOutputButton->setChecked(true);
    LaunchProgram("dots_configuration_check", {"--compare-generated"});
}

void MainWindow::OnShowTypesystemDetailsPressed()
{
    m_ui->showOutputButton->setChecked(true);
    LaunchProgram("dots_configuration_check",{"-d"});
}

void MainWindow::OnTypeIdLookupPressed()
{
    if (! m_ui->lookupEdit->text().isEmpty())
    {
        m_ui->showOutputButton->setChecked(true);
        LaunchProgram("dots_configuration_check",{"--type-id", m_ui->lookupEdit->text().toStdString()});
    }
}

void MainWindow::OnTypeLookupPressed()
{
    if (! m_ui->lookupEdit->text().isEmpty())
    {
        m_ui->showOutputButton->setChecked(true);
        LaunchProgram("dots_configuration_check",{"--type", m_ui->lookupEdit->text().toStdString()});
    }
}

void MainWindow::OnShowOutputToggled(const bool checked)
{
    m_ui->outputTabs->setVisible(checked);
    m_ui->buttonSeparator->setVisible(!checked);
    adjustSize();
}

void MainWindow::OnCloseTab(const int index)
{
    m_ui->outputTabs->setTabVisible(index, false);
}

void MainWindow::OnAllocatePressed()
{
    m_ui->showOutputButton->setChecked(true);
    LaunchProgram("safir_memory_allocator", {"--allocate", m_ui->allocationLevelCombo->currentText().toStdString()});
}

void MainWindow::OnDeallocatePressed()
{
    m_ui->showOutputButton->setChecked(true);
    LaunchProgram("safir_memory_allocator", {"--deallocate"});
}


void MainWindow::AppendText(QTextEdit* textEdit, const QString& string)
{
    auto oldCursor = textEdit->textCursor();
    textEdit->moveCursor (QTextCursor::End);
    textEdit->setTextColor(Qt::black);
    textEdit->insertPlainText(string);
    textEdit->setTextCursor(oldCursor);
}


void MainWindow::AppendMetaText(QTextEdit* textEdit, const QString& string, const bool error)
{
    auto oldCursor = textEdit->textCursor();
    textEdit->moveCursor (QTextCursor::End);
    if (error)
    {
        textEdit->setTextColor(Qt::darkRed);
    }
    else
    {
        textEdit->setTextColor(Qt::darkGray);
    }

    textEdit->insertPlainText(string);
    textEdit->setTextCursor(oldCursor);
}
