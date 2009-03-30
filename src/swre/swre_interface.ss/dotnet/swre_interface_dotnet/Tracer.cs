/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
using System;
using Safir.SwReports;

namespace Safir.Application
{
    /// <summary>
    /// A class for trace logging.
    /// </summary>
    public class Tracer : System.IO.StreamWriter
    {
        /// <summary>
        /// Create a logger with a certain prefix.
        /// </summary>
        /// <param name="prefix">The prefix for this logger.</param>
        public Tracer(string prefix)
            : base(new TraceStream(prefix),new System.Text.UTF8Encoding(false)) //Omit BOM
        {
            AutoFlush = true;
            byte success;
            Library.SwreC_SetProgramName(System.Diagnostics.Process.GetCurrentProcess().ProcessName, out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }
     
        /// <summary>
        /// Check/Set whether a certain prefix is enabled or not.
        /// </summary>
        public bool Enabled
        {
            get
            {
                return Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf
                    (Library.SwreC_TracePrefixIsEnabled(((TraceStream)base.BaseStream).GetPrefixId()));
            }

            set
            {
                byte success;
                Library.SwreC_TracePrefixSetEnabled(((TraceStream)base.BaseStream).GetPrefixId(),
                                                    Safir.Dob.Typesystem.Internal.InternalOperations.ByteOf(value),
                                                    out success);

                if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
                {
                    Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
                }
            }
        }
        
        /// <summary>
        /// Flush the buffered data to the logging output.
        /// </summary>
        public override void Flush()
        {
            if (Enabled)
            {
                base.Flush();
                ((TraceStream)base.BaseStream).ForceFlush();
            }
        }
        
        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(string value)
        {
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(char[] buffer, int index, int count) 
        {
            if (Enabled)
            {
                base.Write(buffer, index, count);
            } 
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(char[] buffer)
        {
            if (Enabled)
            {
                base.Write(buffer);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(char value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(string format, params object[] arg)
        {
            if (Enabled)
            {
                base.Write(format, arg);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(string format, object arg0, object arg1, object arg2)
        {
            if (Enabled)
            {
                base.Write(format, arg0, arg1, arg2);
            } 
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(string format, object arg0, object arg1)
        {
            if (Enabled)
            {
                base.Write(format, arg0, arg1);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(string format, object arg0)
        {
            if (Enabled)
            {
                base.Write(format, arg0);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(object value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(decimal value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(double value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(float value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(ulong value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(long value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(uint value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(int value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void Write(bool value) 
        { 
            if (Enabled)
            {
                base.Write(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(string value)
        {
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        
        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(char[] buffer, int index, int count) 
        {
            if (Enabled)
            {
                base.WriteLine(buffer, index, count);
            } 
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(char[] buffer)
        {
            if (Enabled)
            {
                base.WriteLine(buffer);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(char value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(string format, params object[] arg)
        {
            if (Enabled)
            {
                base.WriteLine(format, arg);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(string format, object arg0, object arg1, object arg2)
        {
            if (Enabled)
            {
                base.WriteLine(format, arg0, arg1, arg2);
            } 
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(string format, object arg0, object arg1)
        {
            if (Enabled)
            {
                base.WriteLine(format, arg0, arg1);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(string format, object arg0)
        {
            if (Enabled)
            {
                base.WriteLine(format, arg0);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(object value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(decimal value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(double value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(float value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(ulong value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(long value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(uint value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(int value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }

        /// <summary>
        /// Override from StreamWriter or TextWriter that 
        /// checks the enabled flag before doing output.
        /// </summary>
        public override void WriteLine(bool value) 
        { 
            if (Enabled)
            {
                base.WriteLine(value);
            }
        }
        
        
    }


    internal class TraceStream : System.IO.Stream
    {
        public TraceStream(string prefix)
        {
            m_prefix = prefix;
        }

        ~TraceStream()
        {
            ForceFlush();
        }

        public System.Int64 GetPrefixId()
        {
            if (m_prefixId == 0)
            {
                AddPrefix();
            }
            return m_prefixId;
        }
        private void AddPrefix()
        {
            byte success;
            
            Library.SwreC_TracePrefixAdd(System.Text.Encoding.UTF8.GetBytes(m_prefix), out m_prefixId, out success);

            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        public override bool CanRead
        {
            get { return false; }
        }

        public override bool CanSeek
        {
            get { return false; }
        }

        public override bool CanWrite
        {
            get { return true; }
        }

        public void ForceFlush()
        {//this is the same as Flush in c++
            byte success;
            Library.SwreC_TraceFlushBuffer(out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        public override void Flush()
        { //this is the same as sync in C++
            byte success;
            Library.SwreC_TraceSyncBuffer(out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        public override long Length
        {
            get { throw new NotSupportedException("The TraceStream class is not seekable."); }
        }

        public override long Position
        {
            get
            {
                throw new NotSupportedException("The TraceStream class is not seekable.");
            }
            set
            {
                throw new NotSupportedException("The TraceStream class is not seekable.");
            }
        }

        public override int Read(byte[] buffer, int offset, int count)
        {
            throw new NotSupportedException("The TraceStream class is not readable.");
        }

        public override long Seek(long offset, System.IO.SeekOrigin origin)
        {
            throw new NotSupportedException("The TraceStream class is not seekable.");
        }

        public override void SetLength(long value)
        {
            throw new NotSupportedException("The TraceStream class is not seekable.");
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            byte success;
            byte[] str = new byte[count];
            Array.Copy(buffer, offset, str, 0, count);
            //buffer.CopyTo(str, offset);
            Library.SwreC_TraceAppendStringPrefix(m_prefixId, str, out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

      

        #region Private stuff

        string m_prefix;
        System.Int64 m_prefixId = 0;

        #endregion
    }
}
