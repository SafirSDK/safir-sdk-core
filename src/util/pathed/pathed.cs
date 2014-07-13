using System;
using System.Collections.Generic;
using GSharpTools;
using System.Text;
using System.IO;
using System.Security;

//namespace pathed
//{
    /// <summary>
    /// This program allows you to view and modify the PATH environment.
    /// </summary>
    class pathed
    {
        /// <summary>
        /// Input arguments handler
        /// </summary>
        private InputArgs Args;

        /// <summary>
        /// Current environment variable type
        /// </summary>
        private EnvironmentVariableTarget EnvironmentVariableTarget = EnvironmentVariableTarget.Process;

        /// <summary>
        /// Name of environment variable, defaults to PATH
        /// </summary>
        private string EnvironmentVariableName = "PATH";

        private string SanitizePath(string path)
        {
            if (path.Equals("."))
                return Environment.CurrentDirectory;
            return path;
        }

        /// <summary>
        /// This program allows you to view and modify the PATH environment.
        /// </summary>
        /// <param name="args"></param>
        private void Run(string[] args)
        {
            Console.OutputEncoding = Encoding.GetEncoding(Encoding.Default.CodePage);
            Args = new InputArgs("pathed", string.Format("Version {0}", AppVersion.Get()) + "\r\nFreeware written by Gerson Kurz (http://p-nand-q.com)");

            Args.Add(InputArgType.Flag, "machine", false, Presence.Optional, "print machine PATH");
            Args.Add(InputArgType.Flag, "user", false, Presence.Optional, "print user PATH");
            Args.Add(InputArgType.ExistingDirectory, "add", "", Presence.Optional, "add variable at the head");
            Args.Add(InputArgType.ExistingDirectory, "append", "", Presence.Optional, "add variable at the tail");
            Args.Add(InputArgType.StringList, "remove", null, Presence.Optional, "remove path / index");
            Args.Add(InputArgType.Flag, "slim", false, Presence.Optional, "strip duplicate vars");
            Args.Add(InputArgType.Parameter, "env", "PATH", Presence.Optional, "environment variable, defaults to PATH");

            if (Args.Process(args))
            {
                EnvironmentVariableName = Args.GetString("env");

                if (Args.GetFlag("slim"))
                    SlimPath();

                if (Args.GetFlag("machine"))
                    EnvironmentVariableTarget = EnvironmentVariableTarget.Machine;

                else if (Args.GetFlag("user"))
                    EnvironmentVariableTarget = EnvironmentVariableTarget.User;

                try
                {
                    List<string> removeItems = Args.GetStringList("remove");
                    if (removeItems != null)
                        Remove(removeItems);

                    string add = Args.GetString("add");
                    if (!string.IsNullOrEmpty(add))
                        AddHead(SanitizePath(add));

                    string append = Args.GetString("append");
                    if (!string.IsNullOrEmpty(append))
                        AddTail(SanitizePath(append));
                }
                catch (SecurityException ex)
                {
                    if (EnvironmentVariableTarget == EnvironmentVariableTarget.Machine)
                    {
                        Console.WriteLine(ex.Message);
                        Console.WriteLine("ERROR, cannot manipulate the machine environment variables, aborting.");
                        return;
                    }
                    else throw;
                }

                ListPath();
            }
        }

        private void SlimPath()
        {
            List<string> vars = new List<string>();

            EnvironmentVariableTarget[] evts = {
                EnvironmentVariableTarget.Machine,
                EnvironmentVariableTarget.User };

            foreach (EnvironmentVariableTarget evt in evts)
            {
                string PathVariable = Environment.GetEnvironmentVariable(EnvironmentVariableName, evt);
                if (PathVariable != null)
                {
                    string[] tokens = PathVariable.Split(';');
                    foreach (string token in tokens)
                    {
                        if (Directory.Exists(token))
                            vars.Add(token);
                    }
                    vars.Add("\u0000");
                }
            }

            StringList.MakeUnique(vars, StringComparison.OrdinalIgnoreCase);

            StringBuilder temp = new StringBuilder();
            bool first = true;
            int evt_index = 0;
            foreach (string s in vars)
            {
                if (s.Equals("\u0000"))
                {
                    Environment.SetEnvironmentVariable(EnvironmentVariableName, temp.ToString(), evts[evt_index++]);
                    temp = new StringBuilder();
                    first = true;
                }
                else
                {
                    if (!first)
                        temp.Append(";");
                    first = false;
                    temp.Append(s);
                }
            }
            Environment.SetEnvironmentVariable(EnvironmentVariableName, temp.ToString(), evts[evt_index++]);
        }

        private void Remove(List<string> items)
        {
            string PathVariable = Environment.GetEnvironmentVariable(EnvironmentVariableName, EnvironmentVariableTarget);
            if (PathVariable != null)
            {
                string[] tokens = PathVariable.Split(';');

                foreach (string remove_this_item in items)
                {
                    int remove_this_index;

                    if (remove_this_item.Equals("invalid", StringComparison.OrdinalIgnoreCase))
                    {
                    }
                    else if (int.TryParse(remove_this_item, out remove_this_index))
                    {
                        if (remove_this_index < tokens.Length)
                            tokens[remove_this_index] = null;
                    }
                    else
                    {
                        int index = 0;
                        foreach (string token in tokens)
                        {
                            if (token.Equals(remove_this_item, StringComparison.OrdinalIgnoreCase))
                            {
                                tokens[index] = null;
                            }
                            ++index;
                        }
                    }
                }

                StringBuilder result = new StringBuilder();
                bool first = true;
                foreach (string token in tokens)
                {
                    if (token != null)
                    {
                        if (!first)
                            result.Append(";");
                        result.Append(token);
                        first = false;
                    }
                }
                Environment.SetEnvironmentVariable(EnvironmentVariableName, result.ToString(), EnvironmentVariableTarget);
            }
        }

        private void AddHead(string var)
        {
            string PathVariable = Environment.GetEnvironmentVariable(EnvironmentVariableName, EnvironmentVariableTarget);
            if (PathVariable != null)
            {
                PathVariable = var + ";" + PathVariable;
                Environment.SetEnvironmentVariable(EnvironmentVariableName, PathVariable, EnvironmentVariableTarget);
            }
        }

        private void AddTail(string var)
        {
            string PathVariable = Environment.GetEnvironmentVariable(EnvironmentVariableName, EnvironmentVariableTarget);
            if (PathVariable != null)
            {
                PathVariable = PathVariable + ";" + var;
                Environment.SetEnvironmentVariable(EnvironmentVariableName, PathVariable, EnvironmentVariableTarget);
            }
        } 

        private void ListPath()
        {
            string PathVariable = Environment.GetEnvironmentVariable(EnvironmentVariableName, EnvironmentVariableTarget);
            if (PathVariable == null)
            {
                Console.WriteLine("ERROR, variable '{0}' does not exist.", EnvironmentVariableName);
            }
            else
            {
                string[] tokens = PathVariable.Split(';');
                int index = 0;
                foreach (string token in tokens)
                {
                    if (string.IsNullOrEmpty(token))
                        continue;
                    if (!Directory.Exists(token))
                    {
                        Console.WriteLine("{0:00} {1} [INVALID]", index, token);
                    }
                    else
                    {
                        Console.WriteLine("{0:00} {1}", index, token);
                    }

                    ++index;
                }
            }
        }

        static void Main(string[] args)
        {
            new pathed().Run(args);           
        }
    }
//}
