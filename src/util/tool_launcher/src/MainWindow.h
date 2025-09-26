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
#include <boost/asio/io_context.hpp>
#include <set>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4251)
#  pragma warning (disable: 4800)
#  pragma warning (disable: 4459)
#  pragma warning (disable: 4018)
#endif

#include <QMessageBox>
#include <QMainWindow>
#include <QTextEdit>
#include <boost/process.hpp>

#ifdef _MSC_VER
#  pragma warning (pop)
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
    void OnLaunchSateLegacyPressed();
    void OnLaunchDobExplorerPressed();
    void OnLaunchControlGuiPressed();
    void OnLaunchTracerViewerButton();
    void OnLaunchBdGuiPressed();
    void OnCheckDouFilesPressed();
    void OnCheckGeneratedPressed();
    void OnShowTypesystemDetailsPressed();
    void OnTypeIdLookupPressed();
    void OnTypeLookupPressed();
    void OnAllocatePressed();
    void OnDeallocatePressed();
    void OnShowOutputToggled(const bool checked);
    void OnCloseTab(const int index);
private:
    static void AppendText(QTextEdit* textEdit, const QString& string);
    static void AppendMetaText(QTextEdit* textEdit, const QString& string, bool error);
    struct Process
    {
        Process(boost::asio::io_context& ioContext)
            : outBuf(4096)
            , errBuf(4096)
            , outPipe(ioContext)
            , errPipe(ioContext)
            , exitError(false)
        {
        }

        ~Process();

        std::vector<char> outBuf;
        std::vector<char> errBuf;
        boost::process::async_pipe outPipe;
        boost::process::async_pipe errPipe;
        std::unique_ptr<boost::process::child> proc;
        QTextEdit* textEdit;
        QString exitText;
        bool exitError;
    };

    void LaunchProgram(const std::string&program, const std::vector<std::string>& args = {});

    void PipeReadOutLoop(const std::shared_ptr<Process>& process);
    void PipeReadErrLoop(const std::shared_ptr<Process>& process);

    void Exited(const std::shared_ptr<Process>& process,
                const std::error_code& error,
                const int exitCode);

    std::unique_ptr<Ui::MainWindow> m_ui;
    boost::asio::io_context m_ioContext;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    std::set<std::shared_ptr<Process>> m_processes;
};
