/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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

#include "common_header.h"
#include "loggingsettings.h"
#include <Safir/Utilities/Internal/LowLevelLoggerControl.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4100)
#endif

#include <boost/program_options.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif



LoggingSettings::LoggingSettings(QWidget* /*parent*/):
    m_timer(this)
{
    setupUi(this);

    CreateControl();
}


void LoggingSettings::LevelChanged(int value)
{
    m_control->LogLevel(value);
}

void LoggingSettings::IgnoreFlush(bool ignore)
{
    m_control->IgnoreFlush(ignore);
}

void LoggingSettings::Timestamps(bool use)
{
    m_control->UseTimestamps(use);
}

void LoggingSettings::Stdout(bool use)
{
    m_control->LogToStdout(use);
}

void LoggingSettings::File(bool /*use*/)
{
    //TODO m_control->LogToFile(use);
}



void LoggingSettings::CreateControl()
{
   try
    {
        m_control.reset(new Safir::Utilities::Internal::LowLevelLoggerControl(true,true));

        /* TODO        if (m_control->Disabled())
        {
            levelGroup->setVisible(false);
            optionsGroup->setVisible(false);
            return;
        }
        else
        {*/
            disabledGroup->setVisible(false);
            //        }

        //only connect these if we have a session to affect immediately
        connect(logLevelSlider, SIGNAL(valueChanged(int)), this, SLOT(LevelChanged(int)));
        connect(ignoreFlush , SIGNAL(toggled(bool)), this, SLOT(IgnoreFlush(bool)));
        connect(timestamps , SIGNAL(toggled(bool)), this, SLOT(Timestamps(bool)));
        connect(toStdout , SIGNAL(toggled(bool)), this, SLOT(Stdout(bool)));
        connect(toFile , SIGNAL(toggled(bool)), this, SLOT(File(bool)));
        connect(&m_timer, SIGNAL(timeout()), this, SLOT(UpdateWidgets()));

        UpdateWidgets();
        m_timer.start(500);
    }
    catch (const std::exception&e)
    {
        QMessageBox::critical(this,"Exception",QString("Got exception from LowLevelLoggerControl:\n") + e.what());
        levelGroup->setEnabled(false);
        optionsGroup->setEnabled(false);
    }
}

void LoggingSettings::UpdateWidgets()
{
    logLevelSlider->setValue(m_control->LogLevel());
    ignoreFlush->setChecked(m_control->IgnoreFlush());
    timestamps->setChecked(m_control->UseTimestamps());
    toStdout->setChecked(m_control->LogToStdout());
    //TODO    toFile->setChecked(m_control->LogToFile());
}
