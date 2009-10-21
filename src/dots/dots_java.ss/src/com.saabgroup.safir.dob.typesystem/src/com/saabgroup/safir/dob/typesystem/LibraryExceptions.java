// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2009 (http://www.safirsdk.com)
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

package com.saabgroup.safir.dob.typesystem;

/**
 * Contains methods used when passing exceptions across language boundaries.
 */
public class LibraryExceptions
{
    //private java.util.HashMap<Long,String> m_exceptionNameTable =
    //    new java.util.HashMap<Long,String>();

    private Exception m_cachedException;

    private LibraryExceptions()
    {

    }

    /**
     * Get the instance of the singleton.
     *
     * @return The instance of the singleton.
     */
    public static LibraryExceptions getInstance()
    {
        return m_instance;
    }

    private String getStackTrace(Throwable exception) {
        java.io.CharArrayWriter chars = new java.io.CharArrayWriter();
        java.io.PrintWriter writer = new java.io.PrintWriter(chars);
        exception.printStackTrace(writer);
        writer.flush();
        return chars.toString();
    }

    private void set(FundamentalException exception)
    {
        set(exception.getTypeId(), getStackTrace(exception));
    }

    private void set(com.saabgroup.safir.dob.typesystem.Exception exception)
    {
        set(exception.getTypeId(), getStackTrace(exception));
    }

    /**
     * Set the current exception.
     *
     * When you have caught an exception that you would like to pass across language
     * boundaries, call this method with the exception.
     * Then, on the other side of the language boundary, call
     * the Throw method, which will throw the exception in the other language.
     *
     * @param exception The exception.
     */
    public void set(java.lang.Exception exception)
    {
        if (exception instanceof com.saabgroup.safir.dob.typesystem.Exception)
        {
            set((com.saabgroup.safir.dob.typesystem.Exception)exception);
            return;
        }

        if (exception instanceof com.saabgroup.safir.dob.typesystem.FundamentalException)
        {
            set((com.saabgroup.safir.dob.typesystem.FundamentalException)exception);
            return;
        }

        set(0, getStackTrace(exception));
    }

    private void set(long exceptionId, String description)
    {
        Kernel.SetException(exceptionId,description);
    }

    /**
     * Check the source code in dose_java for examples of how to use this.
     * @return difficult to explain...
     */
    public com.saabgroup.safir.dob.typesystem.Exception getException()
    {
        if (m_cachedException == null)
        {
            throw new SoftwareViolationException("getException: There was no cached exception! There is an error in the library that you just called!");
        }
        else
        {
            com.saabgroup.safir.dob.typesystem.Exception exc = m_cachedException;
            m_cachedException = null;
            return exc;
        }
    }

    /**
     * Check the source code in dose_java for examples of how to use this.
     */
    public void throwUnknown()
    {
        if (m_cachedException == null)
        {
            throw new SoftwareViolationException("throwUnknown: There was no cached exception! There is an error in the library that you just called!"
                                                 + " Contact the developer of the library so that he/she can fix it");
        }
        else
        {
            com.saabgroup.safir.dob.typesystem.Exception exc = m_cachedException;
            m_cachedException = null;
            throw new SoftwareViolationException("The exception '"
                                                 + exc.getClass().getName()
                                                 + "' was not handled correctly in the library that you just called"
                                                 + " Contact the developer of the library so that he/she can fix it.\n"
                                                 + "Exception.toString: " + exc.toString());
        }
    }


    /**
     * Check the source code in dose_java for examples of how to use this.
     */
    public void throwFundamental()
    {
        boolean [] wasSet = new boolean [1];
        long [] exceptionId = new long [1];
        String [] description = new String [1];
        Kernel.GetAndClearException(exceptionId, description, wasSet);

        if (wasSet[0])
        {
            if (exceptionId[0] == 0)
            {
                throw new java.lang.RuntimeException(description[0]);
            }
            else
            {
                String typename = getClassName(exceptionId[0]);
                if (typename != null)
                { //found in table

                    FundamentalException exc;
                    try
                    {
                        java.lang.Object obj = Class.forName(typename).getConstructor(String.class).newInstance(description[0]);
                        if (obj instanceof Exception)
                        {
                            m_cachedException = (Exception)obj;
                            return;
                        }
                        exc = (FundamentalException)obj;

                    }
                    catch (java.lang.Exception e)
                    {
                        throw new IllegalValueException("Could not create exception " + typename);
                    }
                    throw exc;
                }
                else
                {
                    throw new SoftwareViolationException("Could not find an exception in the exception table! exceptionId = "
                                                         + exceptionId[0] + " description = " + description[0]);
                }
            }
        }
        else
        {
            throw new SoftwareViolationException("There was no exception set when LibraryExceptions.throw was called!");
        }
    }

    private String getClassName(long typeId){
        String fullClassName = Operations.getName(typeId);

        //Object is a "special name".
        if (fullClassName.equals("Exception")) {
            fullClassName = "Safir.Dob.Typesystem.Exception";
        }
        if (fullClassName.equals("FundamentalException")) {
            fullClassName = "Safir.Dob.Typesystem.FundamentalException";
        }

        //Adjust the namespace to lowercase.
        int lastDot = fullClassName.lastIndexOf('.');
        String className = fullClassName.substring(lastDot);
        String namespace = fullClassName.substring(0,lastDot);
        String adjustedClassName = namespace.toLowerCase() + className;

        //add com.saabgroup
        return "com.saabgroup." + adjustedClassName;
    }

    private static final LibraryExceptions m_instance = new LibraryExceptions();

}
