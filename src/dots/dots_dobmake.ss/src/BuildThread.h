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
#pragma once

#include <boost/current_function.hpp>
#include <iostream>

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
                const QString& installDir)
        : QThread(parent)
        , m_dobmakeBatchScript(dobmakeBatchScript)
        , m_buildDir(buildDir)
        , m_debug(debug)
        , m_release(release)
        , m_installDir(installDir)
    {
        std::cerr << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
    }

signals:
    void BuildComplete(const bool result);
private:
    void run()
    {
        std::cerr << "Entering " << BOOST_CURRENT_FUNCTION << std::endl;
        QStringList params;

        params << m_dobmakeBatchScript;
        params << "--skip-tests";

#if QT_VERSION >= 0x050000
        std::cerr << "qt5" <<std::endl;
        const QString null = QProcess::nullDevice();
#endif

#if defined(linux) || defined(__linux) || defined(__linux__)
        params << "--config";
#  if QT_VERSION < 0x050000
        std::cerr << "qt4" <<std::endl;
        const QString null("/dev/null");
#  endif

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        params << "--configs";
#  if QT_VERSION < 0x050000
        std::cerr << "qt4" <<std::endl;
        const QString null("nul");
#  endif
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

        if (!m_installDir.isEmpty())
        {
            params << "--install" << m_installDir;
        }

        QProcess p;
        p.setWorkingDirectory(m_buildDir);
        p.setStandardOutputFile(null);
        p.setStandardErrorFile(null);
        p.start("python", params);
        p.waitForFinished(-1);

        emit BuildComplete(p.error() == QProcess::UnknownError &&
                           p.exitStatus() == QProcess::NormalExit &&
                           p.exitCode() == 0);
    }

    const QString m_dobmakeBatchScript;
    const QString m_buildDir;
    const bool m_debug;
    const bool m_release;
    const QString m_installDir;
};
