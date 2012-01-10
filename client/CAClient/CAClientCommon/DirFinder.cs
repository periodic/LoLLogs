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
        public abstract List<string> getDirs();
        public abstract string getDirectory();
 
        protected static List<string> getLogDirs(string path) 
        {
            var logDirs = new List<string>();

            if (! Directory.Exists(path))
                throw new DirectoryNotFoundException("Base directory does not exist: " + path);

            if (!Directory.Exists(path + releaseDir))
                throw new DirectoryNotFoundException("Could not find release directory: " + path + releaseDir);

            foreach (string verDir in Directory.EnumerateDirectories(path + releaseDir))
            {
                string logDir = verDir + logSubDir;
                if (Directory.Exists(logDir))
                    logDirs.Add(logDir);
                else
                    throw new DirectoryNotFoundException("Directory should exist: " + logDir);
            }

            return logDirs;
        }
        protected static List<string> getLogFiles(string path)
        {
            var logFiles = new List<string>();

            var logDirs = getLogDirs(path);

            foreach (string logDir in logDirs)
            {
                logFiles.AddRange(Directory.EnumerateFiles(logDir));
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

        public override List<string> getDirs()
        {
            requestDirectory();
            return getLogDirs(baseDir);
        }

        public override string getDirectory()
        {
            return baseDir;
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

        public override List<string> getDirs()
        {
            return getLogDirs(directory);
        }

        public override string getDirectory()
        {
            return directory;
        }
    }

    public class RegistryDirFinder : DirFinder
    {
        public RegistryKey registryKey;
        public string registryValue;

        public RegistryDirFinder(RegistryKey hive, string key, string val)
        {

            registryKey = hive.OpenSubKey(key);
            registryValue = val;
        }

        public override List<string> getFiles()
        {
            return getLogFiles(lookupRegistryString());
        }

        public override List<string> getDirs()
        {
            return getLogDirs(lookupRegistryString());
        }

        private string lookupRegistryString()
        {
            if (registryKey == null)
            {
                throw new DirectoryNotFoundException("The registry subkey does not exist.");
            }

            Object val = registryKey.GetValue(registryValue, null);

            if (val == null)
                throw new DirectoryNotFoundException("Could not get directory from registry at: " + registryKey.Name + " : " + registryValue);

            try {
                return (string)val;
            } catch (InvalidCastException) {
                throw new DirectoryNotFoundException("Registry did not return a string: " + registryKey.Name + " : " + registryValue);
            }
        }

        public override string getDirectory()
        {
            return lookupRegistryString();
        }
    }
}
