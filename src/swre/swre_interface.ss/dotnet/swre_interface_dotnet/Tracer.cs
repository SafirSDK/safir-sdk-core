/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://www.safirsdk.com)
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
    /// This class just contains two static methods, for starting and stopping the tracers backdoor.
    /// </summary>
    public sealed class TracerBackdoor
    {
        /// <summary>
        /// Start reception of trace on/off commands
        /// <para/>
        /// The given connection must be opened before this method is called.
        /// If the connection is closed the reception of backdoor commands is
        /// stopped. If a new connection is opened this method needs to be called
        /// in order to start reception of backdoor commands.
        /// <para/>
        /// In situations when a connection is regularly closed and reopened,
        /// for instance in the case of context switches, you should consider
        /// using a dedicated connection as parameter.
        /// </summary>
        /// <param name="connection">The connection used for setting up a subscription for
        ///                          backdoor commands.</param>
        public static void Start(Safir.Dob.ConnectionBase connection)
        {
            byte success;
            Library.SwreC_SetProgramName(System.Text.Encoding.UTF8.GetBytes
                                         (System.Diagnostics.Process.GetCurrentProcess().ProcessName) + char.MinValue,
                                         out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }

            Safir.Dob.ConnectionAspectMisc misc = new Safir.Dob.ConnectionAspectMisc(connection);

            Library.SwreC_StartTraceBackdoor(System.Text.Encoding.UTF8.GetBytes(misc.GetConnectionNameCommonPart() + char.MinValue),
                                             System.Text.Encoding.UTF8.GetBytes(misc.GetConnectionNameInstancePart() + char.MinValue),
                                             out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        ///<summary>
        /// Stop reception of trace on/off commands
        ///</summary>
        static void Stop()
        {
            Library.SwreC_StopTraceBackdoor();
        }
    }

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
            AddPrefix();
        }

        ~TraceStream()
        {

        }

        public System.Int64 GetPrefixId()
        {
            return m_prefixId;
        }

        private void AddPrefix()
        {
            byte success;
            
            Library.SwreC_TracePrefixAdd(System.Text.Encoding.UTF8.GetBytes(m_prefix + char.MinValue), out m_prefixId, out success);

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

        public override void Flush()
        { //this is the same as sync in C++
            byte success;
            Library.SwreC_TraceFlush(out success);
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
            //byte[] str = new byte[count + 1];
            //Array.Copy(buffer, offset, str, 0, count);
            //str[count] = 0; //add null termination
            Library.SwreC_TraceAppendSubstring(m_prefixId, buffer, offset, count, out success);
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
