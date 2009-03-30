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
package safir.application;


public class Tracer 
    extends java.io.PrintWriter
{
    public Tracer(String prefix)
    {
        super(new java.io.StringWriter(),true);
        try
        {
            m_traceStream = new TraceStream(prefix);
            super.out = new java.io.BufferedWriter(new java.io.OutputStreamWriter(m_traceStream,"UTF-8"));
        }
        catch (java.io.UnsupportedEncodingException exc)
        {
            //UTF-8 is guaranteed to be supported by the language.
        }
    }
    
    static public void setProgramName(String programName)                                     
    {
        boolean [] success = new boolean [1];
        
        Library.SetProgramName(programName, success);
        if (!success[0])
        {
            safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    public boolean isEnabled()
    {
        return Library.TracePrefixIsEnabled(m_traceStream.getPrefixId());
    }
    
    public void setEnabled(boolean enabled)
    {
        boolean [] success = new boolean [1];
        Library.TracePrefixSetEnabled(m_traceStream.getPrefixId(),
                                      enabled,
                                      success);
        if (!success[0])
        {
            safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    public java.io.PrintWriter append(char c)
    {
        if (isEnabled())
        {
            super.append(c);
        }
        return this;
    }
    
    public java.io.PrintWriter append(CharSequence csq)
    {
        if (isEnabled())
        {
            super.append(csq);
        }
        return this;
    }

    public java.io.PrintWriter append(CharSequence csq, int start, int end)
    {
        if (isEnabled())
        {
            super.append(csq,start,end);
        }
        return this;
    }
    /*
      public void close()
      {
      if (isEnabled())
      {
            
      }
      }*/
    
      public void flush()
      {
          if (isEnabled())
          {
              boolean [] success = new boolean [1];
              Library.TraceFlushBuffer(success);
              if (!success[0])
              {
                  safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                  safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
              }
          }
      }

    public java.io.PrintWriter format(java.util.Locale l, String format, Object... args)
    {
        if (isEnabled())
        {
            super.format(l,format,args);
        }
        return this;
    }

    public java.io.PrintWriter format(String format, Object... args)
    {
        if (isEnabled())
        {
            super.format(format,args);
        }
        return this;
    }

    public void print(boolean b)
    {
        if (isEnabled())
        {
            super.print(b);
        }
    }

    public void print(char c)
    {
        if (isEnabled())
        {
            super.print(c);
        }
    }

    public void print(char[] s)
    {
        if (isEnabled())
        {
            super.print(s);
        }
    }

    public void print(double d)
    {
        if (isEnabled())
        {
            super.print(d);
        }
    }

    public void print(float f)
    {
        if (isEnabled())
        {
            super.print(f);
        }
    }

    public void print(int i)
    {
        if (isEnabled())
        {
            super.print(i);
        }
    }

    public void print(long l)
    {
        if (isEnabled())
        {
            super.print(l);
        }
    }

    public void print(Object obj)
    {
        if (isEnabled())
        {
            super.print(obj);
        }
    }

    public void print(String s)
    {
        if (isEnabled())
        {
            super.print(s);
        }
    }

    public java.io.PrintWriter printf(java.util.Locale l, String format, Object... args)
    {
        if (isEnabled())
        {
            super.printf(l,format,args);
        }
        return this;
    }

    public java.io.PrintWriter printf(String format, Object... args)
    {
        if (isEnabled())
        {
            super.printf(format,args);
        }
        return this;
    }

    public void println()
    {
        if (isEnabled())
        {
            super.println();
        }
    }

    public void println(boolean x)
    {
        if (isEnabled())
        {
            super.println(x);
        }
    }

    public void println(char x)
    {
        if (isEnabled())
        {
            super.println(x);
        }
    }

    public void println(char[] x)
    {
        if (isEnabled())
        {
            super.println(x);
        }
    }

    public void println(double x)
    {
        if (isEnabled())
        {
            super.println(x);
        }
    }

    public void println(float x)
    {
        if (isEnabled())
        {
            super.println(x);
        }
    }

    public void println(int x)
    {
        if (isEnabled())
        {
            super.println(x);
        }
    }

    public void println(long x)
    {
        if (isEnabled())
        {
            super.println(x);
        }
    }

    public void println(Object x)
    {
        if (isEnabled())
        {
            super.println(x);
        }
    }

    public void println(String x)
    {
        if (isEnabled())
        {
            super.println(x);
        }
    }

    public void write(char[] buf)
    {
        if (isEnabled())
        {
            super.write(buf);
        }
    }

    public void write(char[] buf, int off, int len)
    {
        if (isEnabled())
        {
            super.write(buf,off,len);
        }
    }

    public void write(int c)
    {
        if (isEnabled())
        {
            super.write(c);
        }
    }

    public void write(String s)
    {
        if (isEnabled())
        {
            super.write(s);
        }
    }

    public void write(String s, int off, int len)
    {
        if (isEnabled())
        {
            super.write(s,off,len);
        }
    }

    

    private TraceStream m_traceStream;
}


class TraceStream
    extends java.io.OutputStream
{

    public TraceStream(String prefix)
    {
        m_prefix = prefix;
    }

    public long getPrefixId()
    {
        if (m_prefixId == 0)
        {
            AddPrefix();
        }
        return m_prefixId;
    }
    
    private void AddPrefix()
    {
        boolean [] success = new boolean [1];
        long [] prefixId = new long [1];
        Library.TracePrefixAdd(m_prefix, prefixId, success);
        
       if (!success[0])
        {
            safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
       m_prefixId = prefixId[0];
    }


    public void close() throws java.io.IOException
    {
        boolean [] success = new boolean [1];
        Library.TraceFlushBuffer(success);
        if (!success[0])
        {
            safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
        super.close();
    }

    public void flush() throws java.io.IOException
    {
        super.flush();
        boolean [] success = new boolean [1];
        Library.TraceSyncBuffer(success);
        if (!success[0])
        {
            safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }
    public void write(byte[] b)
    {
        boolean [] success = new boolean [1];
        Library.TraceAppendStringPrefix(m_prefixId,b,success);
        if (!success[0])
        {
            safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }
    
    public void write(int b) 
    {
        boolean [] success = new boolean [1];
        Library.TraceAppendCharPrefix(m_prefixId,(byte)(b & 0xFF),success);
        if (!success[0])
        {
            safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    private String m_prefix;
    private long m_prefixId = 0;
}
