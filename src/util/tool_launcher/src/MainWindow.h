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
#pragma once

#include <memory>
#include <boost/process.hpp>
#include <boost/asio/io_service.hpp>
#include <set>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#pragma warning (disable: 4251)
#pragma warning (disable: 4800)
#endif

#include <QMessageBox>
#include <QMainWindow>
#include <QPlainTextEdit>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow();
    ~MainWindow();

private slots:
    void OnLaunchSatePressed();
    void OnLaunchDobExplorerPressed();
    void OnShowOutputToggled(const bool checked);
private:
    static void AppendText(QPlainTextEdit* textEdit, const QString& string);
    struct Process
    {
        Process(boost::asio::io_service& ioService)
            : outBuf(4096)
            , errBuf(4096)
            , outPipe(ioService)
            , errPipe(ioService)
        {
        }

        std::vector<char> outBuf;
        std::vector<char> errBuf;
        boost::process::async_pipe outPipe;
        boost::process::async_pipe errPipe;
        std::unique_ptr<boost::process::child> proc;
        QPlainTextEdit* textEdit;
    };

    void LaunchProgram(const std::string&program);

    void PipeReadOutLoop(const std::shared_ptr<Process>& process);
    void PipeReadErrLoop(const std::shared_ptr<Process>& process);

    void Exited(const std::shared_ptr<Process>& process,
                const std::error_code& error,
                const int exitCode);

    std::unique_ptr<Ui::MainWindow> m_ui;
    boost::asio::io_service m_ioService;
    std::unique_ptr<boost::asio::io_service::work> m_work;
    std::set<std::shared_ptr<Process>> m_processes;
};
