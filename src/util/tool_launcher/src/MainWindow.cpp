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
#pragma warning (push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#pragma warning (disable: 4251)
#pragma warning (disable: 4800)
#endif

#include "ui_MainWindow.h"
#include <QPushButton>
#include <QTimer>
#include <QShortcut>
#include <QScrollBar>

#ifdef _MSC_VER
#pragma warning (pop)
#endif


MainWindow::MainWindow()
    : QMainWindow(nullptr)
    , m_ui(std::make_unique<Ui::MainWindow>())
    , m_work(std::make_unique<boost::asio::io_service::work>(m_ioService))
{
    m_ui->setupUi(this);
    m_ui->outputTabs->setVisible(false);
    adjustSize();
    auto* shortcut = new QShortcut(QKeySequence::Cancel, this);
    connect(shortcut, &QShortcut::activated, this, &QMainWindow::close);

    connect(m_ui->launchSateButton, &QPushButton::pressed, this, &MainWindow::OnLaunchSatePressed);
    connect(m_ui->launchDobExplorerButton, &QPushButton::pressed, this, &MainWindow::OnLaunchDobExplorerPressed);
    connect(m_ui->launchEntityViewerButton, &QPushButton::pressed, this, &MainWindow::OnLaunchEntityViewerPressed);
    connect(m_ui->launchControlGuiButton, &QPushButton::pressed, this, &MainWindow::OnLaunchControlGuiPressed);
    connect(m_ui->showOutputButton, &QAbstractButton::toggled, this, &MainWindow::OnShowOutputToggled);

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


void MainWindow::LaunchProgram(const std::string& program)
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
         m_ioService,
         error,
         boost::process::on_exit=[this,process](int exitCode, const std::error_code& error){Exited(process, error, exitCode);},
         boost::process::std_out > process->outPipe,
         boost::process::std_err > process->errPipe,
         environment
         );

    process->textEdit = new QPlainTextEdit(this);
    process->textEdit->setReadOnly(true);

    const int index = m_ui->outputTabs->
        addTab(process->textEdit, QString::fromStdString(program) + " " + QString::number(process->proc->id()));
    m_ui->outputTabs->setCurrentIndex(index);
    AppendText(process->textEdit,
               tr("Output from '%1' with pid %2 and SAFIR_INSTANCE=%3:\n")
                 .arg(QString::fromStdString(program))
                 .arg(process->proc->id())
                 .arg(m_ui->safirInstanceSpin->value()));

    if (error)
    {
        AppendText(process->textEdit, tr("Failed to launch process: %1\n").arg(QString::fromStdString(error.message())));
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
             if (error == boost::asio::error::eof)
             {
                 return;
             }
             else if (error)
             {
                 AppendText(process->textEdit, tr("Failed to read process output: %1\n").arg(QString::fromStdString(error.message())));
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
             if (error == boost::asio::error::eof)
             {
                 return;
             }
             else if (error)
             {
                 AppendText(process->textEdit, tr("Failed to read process error: %1\n").arg(QString::fromStdString(error.message())));
                 return;
             }

             AppendText(process->textEdit,QString::fromUtf8(process->errBuf.data(), static_cast<int>(size)));

             PipeReadErrLoop(process);
      });
}

void MainWindow::Exited(const std::shared_ptr<Process>& process,
                        const std::error_code& error,
                        const int exitCode)
{
    if (error)
    {
        AppendText(process->textEdit, tr("Error in waiting for process to exit: %1\n").arg(QString::fromStdString(error.message())));
        return;
    }

    AppendText(process->textEdit, tr("Process exited with code %1").arg(exitCode));

    m_processes.erase(process);
}


void MainWindow::OnLaunchSatePressed()
{
    LaunchProgram("sate");
}

void MainWindow::OnLaunchDobExplorerPressed()
{
    LaunchProgram("dobexplorer");
}

void MainWindow::OnLaunchEntityViewerPressed()
{
    LaunchProgram("safir_entity_viewer");
}

void MainWindow::OnLaunchControlGuiPressed()
{
    LaunchProgram("safir_control_gui");
}

void MainWindow::OnShowOutputToggled(const bool checked)
{
    m_ui->outputTabs->setVisible(checked);
    adjustSize();
}

void MainWindow::AppendText(QPlainTextEdit* textEdit, const QString& string)
{
    //append new text
    textEdit->appendPlainText(string);

    //scroll to bottom
    textEdit->verticalScrollBar()->setValue(textEdit->verticalScrollBar()->maximum());
}
