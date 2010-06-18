/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
* 
* Created by: Henrik Sundberg / sthesu
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

using System;
using System.Collections.Generic;
using System.Text;

namespace dose_test_dotnet
{
    internal sealed class Logger : System.IO.TextWriter
    {
        static readonly Logger instance = new Logger();

        static Logger()
        {

        }

        public static Logger Instance
        {
            get
            {
                return instance;
            }
        }

        private Logger()
        {

        }

        public override Encoding Encoding { get { return m_buf.Encoding; } }

        public override void Close() { m_buf.Close(); }
        
        public override void Write(char value)
        {
            System.Console.Write(value);
            m_buf.Write(value);
        }
        public override void Write(string value)
        {
            System.Console.Write(value);
            m_buf.Write(value);
        }

        public override void Write(char[] buffer, int index, int count)
        {
            System.Console.Write(buffer, index, count);
            m_buf.Write(buffer, index, count);
        }

        public override void WriteLine(string value)
        {
            this.Write(value + "\n");
        }

        public override void WriteLine()
        {
            this.Write("\n");
        }

        public override string ToString()
        {
            return m_buf.ToString();
        }

        public void Clear()
        {
            m_buf = new System.IO.StringWriter();
        }

        private System.IO.StringWriter m_buf = new System.IO.StringWriter();
    }
}
