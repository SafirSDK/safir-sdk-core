/******************************************************************************
*
* Copyright Saab AB, 2014-2015 (http://safir.sourceforge.net)
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
#pragma once

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#pragma warning (disable: 4251)
#endif

#include <QThread>
#include <QProcess>
#include <QFile>

#ifdef _MSC_VER
#pragma warning(pop)
#endif


class BuildThread : public QThread
{
    Q_OBJECT
public:
    BuildThread(QObject* parent,
                const QString& dobmakeBatchScript,
                const QString& buildDir,
                const bool debug,
                const bool release,
                const bool force32Bit, //adds --32-bit to cmd line
                const QString& installDir)
        : QThread(parent)
        , m_dobmakeBatchScript(dobmakeBatchScript)
        , m_buildDir(buildDir)
        , m_debug(debug)
        , m_release(release)
        , m_force32Bit(force32Bit)
        , m_installDir(installDir)
    {

    }

signals:
    void BuildComplete(const bool result);

private:
    void run()
    {
        QStringList params;

        params << m_dobmakeBatchScript;
        params << "--skip-tests";
        params << "--verbose";

#if defined(linux) || defined(__linux) || defined(__linux__)
        params << "--config";
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        params << "--configs";
#else
#  error Dobmake does not know how to handle this platform
#endif

        if (m_debug)
        {
            params << "Debug";
        }

        if (m_release)
        {
            params << "RelWithDebInfo";
        }

        if (m_force32Bit)
        {
            params << "--32-bit";
        }

#if _MSC_VER == 1600
        params << "--use-studio" << "2010";
#elif _MSC_VER == 1700
        params << "--use-studio" << "2012";
#elif _MSC_VER == 1800
        params << "--use-studio" << "2013";
#elif defined(_MSC_VER)
#  error "Unknown version of Visual Studio. Dobmake won't stand for it!"
#endif

        if (!m_installDir.isEmpty())
        {
            params << "--install" << m_installDir;
        }

        QProcess process;
        process.setWorkingDirectory(m_buildDir);
        process.closeReadChannel(QProcess::StandardOutput);
        process.closeReadChannel(QProcess::StandardError);
        process.start("python", params);
        process.waitForFinished(-1);

        emit BuildComplete(process.error() == QProcess::UnknownError &&
                           process.exitStatus() == QProcess::NormalExit &&
                           process.exitCode() == 0);
    }

    const QString m_dobmakeBatchScript;
    const QString m_buildDir;
    const bool m_debug;
    const bool m_release;
    const bool m_force32Bit;
    const QString m_installDir;
};
