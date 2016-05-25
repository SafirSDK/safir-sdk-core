using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Windows.Forms;
using System.Xml.Serialization;

namespace Sate
{
    public class Application
    {
        public string Description { get; set; }
        public string Name { get; set; }
        public string Arguments { get; set; }
    }

    [XmlRoot("ExternalApplicationSettings", Namespace = "", IsNullable = true)]
    public class ExternalApplicationSettings
    {

        public List<Application> Applications = new List<Application>();

        [XmlIgnore]
        private static string _filenameCache;

        public static string GetFilename()
        {
            if (_filenameCache != null)
            {
                return _filenameCache;
            }

            try
            {
                var info = new ProcessStartInfo("safir_show_config", "--config-path")
                {
                    UseShellExecute = false,
                    RedirectStandardOutput = true
                };
                var proc = Process.Start(info);
                // Read the output stream first and then wait.
                string output = proc.StandardOutput.ReadToEnd().Trim();
                proc.WaitForExit();

                _filenameCache = Path.Combine(output, "sate_external_applications.xml");
            }
            catch
            {
                _filenameCache = "<failed to find>";
            }

            return _filenameCache;
        }

        public static ExternalApplicationSettings Settings { get; set; }

        public static void Load()
        {
            var serializer = new XmlSerializer(typeof(ExternalApplicationSettings));

            try
            {
               
                if (File.Exists(GetFilename()))
                {
                    using (var fs = new FileStream(GetFilename(), FileMode.Open))
                    {
                        Settings = (ExternalApplicationSettings)serializer.Deserialize(fs);
                    }
                }
                else
                {
                    Settings = new ExternalApplicationSettings();
                }
            }
            catch
            {
                MessageBox.Show(
                    @"The file " + GetFilename() + @" is corrupt or obsolete, please fix.",
                    @"Invalid Settings");

                Settings = new ExternalApplicationSettings();
            }
        }

        public static void Save()
        {
            try
            {
                var serializer = new XmlSerializer(typeof(ExternalApplicationSettings));
                using (TextWriter writer = new StreamWriter(GetFilename()))
                {
                    serializer.Serialize(writer, Settings);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.InnerException);
            }
        }
    }
}
