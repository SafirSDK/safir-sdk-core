/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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

namespace dose_test_dotnet
{
    class Program
    {
        static int Main(string[] args)
        {
            System.Console.WriteLine("Starting CrashReporter");
            Safir.Application.CrashReporter.Start();

            System.Console.WriteLine("Starting");
            try
            {
                Executor app = new Executor(args);
                app.Run();
            }
            catch (System.Exception e)
            {
                Logger.Instance.WriteLine("Caught Exception! Contents of exception is:");
                Logger.Instance.WriteLine(e);
                return 12;
            }
            finally
            {
                Safir.Application.CrashReporter.Stop();
            }

            System.Console.WriteLine("Exiting");
            return 0;
        }
    }
}
