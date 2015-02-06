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
#ifndef LOGGING_SETTINGS_H
#define LOGGING_SETTINGS_H
#include "common_header.h"
#include "ui_loggingsettings.h"
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>

//forward declaration
namespace Safir { namespace Utilities { namespace Internal {
    class LowLevelLoggerControl;
}}}

class LoggingSettings :
  public QWidget,
  private Ui::LoggingSettings,
  private boost::noncopyable
{
    Q_OBJECT

public:
    LoggingSettings(QWidget *parent = 0);


public slots:
    void LevelChanged(int value);
    void IgnoreFlush(bool ignore);
    void Timestamps(bool use);
    void Stdout(bool use);

    void UpdateWidgets();
private:
    void CreateControl();
    QTimer m_timer;

    boost::shared_ptr<Safir::Utilities::Internal::LowLevelLoggerControl> m_control;
};


#endif
