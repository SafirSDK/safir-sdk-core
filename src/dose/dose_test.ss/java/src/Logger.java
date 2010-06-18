// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2006-2009 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

class Logger
{
    private static Logger m_instance = null;

    public static Logger instance()
    {
        if (m_instance == null)
        {
            m_instance = new Logger();
        }
        return m_instance;
    }

    Logger()
    {
        m_stream = new java.io.PrintStream(m_buf);
    }
    public void println()
    {
        print("\n");
    }

    public void println(String str)
    {
        print(str + "\n");
    }

    public void print(String str)
    {
        m_stream.print(str);
        System.out.print(str);
    }

    public String toString()
    {
        return m_buf.toString();
    }

    public void clear()
    {
        m_buf = new ByteArrayOutputStream();
        m_stream = new java.io.PrintStream(m_buf);
    }

    PrintStream m_stream;
    ByteArrayOutputStream m_buf = new ByteArrayOutputStream();
}
