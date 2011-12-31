using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Win32;
using System.Windows.Forms;

namespace CAClientCommon
{
    public abstract class DirFinder
    {
        static string releaseDir = "\\RADS\\projects\\lol_air_client\\releases";
        static string logSubDir = "\\deploy\\logs";

        public abstract List<string> getFiles();
        // public abstract bool hasFiles();
 
        protected static List<string> getLogFiles(string path)
        {
            var logFiles = new List<string>();

            if (! Directory.Exists(path))
                throw new DirectoryNotFoundException("Base directory does not exist: " + path);

            if (!Directory.Exists(path + releaseDir))
                throw new DirectoryNotFoundException("Could not find release directory: " + path + releaseDir);

            foreach (string verDir in Directory.EnumerateDirectories(path + releaseDir))
            {
                string logDir = verDir + logSubDir; // The enumerator returns directories with a trailing slash.
                if (Directory.Exists(logDir))
                    logFiles.AddRange(Directory.EnumerateFiles(logDir));
                else
                    throw new DirectoryNotFoundException("Directory should exist: " + logDir);
            }

            if (logFiles.Count == 0)
                throw new DirectoryNotFoundException("Could not find any log files: " + path + releaseDir);

            return logFiles;
        }
    }

    public class DialogDirFinder : DirFinder
    {
        public DialogDirFinder() { }

        string baseDir;
        bool requested = false;

        private void requestDirectory()
        {
            if (requested == false) {
                requested = true;
                var fbd = new FolderBrowserDialog();
                fbd.ShowNewFolderButton = false;
                fbd.Description = "Select your LoL Folder";
                fbd.RootFolder = System.Environment.SpecialFolder.MyComputer;
    
                DialogResult result = fbd.ShowDialog();
    
                if (result == DialogResult.OK)
                {
                    baseDir = fbd.SelectedPath;
                } else {
                    throw new DirectoryNotFoundException("User did not supply a directory.");
                }
            }
        }

        public override List<string> getFiles()
        {
            requestDirectory();
            return getLogFiles(baseDir);
        }
    }

    public class StaticDirFinder : DirFinder{
        public string directory;

        public StaticDirFinder(string dir) {
            directory = dir;
        }

        public override List<string> getFiles() {
            return getLogFiles(directory);
        }
    }

    public class RegistryDirFinder : DirFinder
    {
        public string registryKey;
        public string registryValue;

        public RegistryDirFinder(string key, string val)
        {
            registryKey = key;
            registryValue = val;
        }

        public override List<string> getFiles()
        {
            return getLogFiles(lookupRegistryString());
        }

        string lookupRegistryString()
        {
            Object val = Registry.GetValue(registryKey, registryValue, null);

            if (val == null)
                throw new DirectoryNotFoundException("Could not get directory from registry at: " + registryKey + " : " + registryValue);

            try {
                return (string)val;
            } catch (InvalidCastException e) {
                throw new DirectoryNotFoundException("Registry did not return a string: " + registryKey + " : " + registryValue);
            }
        }
    }
}
