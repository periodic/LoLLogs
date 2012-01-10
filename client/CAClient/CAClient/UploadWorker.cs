using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.ComponentModel;
using System.Timers;
using System.Threading;
using CAClientCommon;
using Microsoft.Win32;

namespace CAClient
{
    public class UploadWorker
    {

        private const double waitInterval = 15 * 60 * 1000; // 15 minutes.
        private DateTime lastProcessed; 
        private List<string> directories;
        // private BackgroundWorker worker;
        private UILogger logger;
        private System.Timers.Timer timer;

        private void log(string msg)
        {
            logger.log(msg);
        }

        public UploadWorker(List<string> dirs, UILogger logger) {
            this.directories = dirs;
            this.logger = logger;
            timer = new System.Timers.Timer(waitInterval);
            timer.AutoReset = false;
            timer.Elapsed += new ElapsedEventHandler((source, e) => start());
        }

        public void start() {
            log("Starting background monitoring.");

            /*
            if (worker.CancellationPending)
            {
                log("Processing cancelled.");
                return;
            }
            */

            lock (this)
            {
                try
                {
                    timer.Stop(); // Stop any currently running timers.
                    ProcessFiles();
                    timer.Start(); // Reset the timer.
                }
                catch (Exception e)
                {
                    log(e.Message);
                }
            }
        }

        public void ProcessFiles()
        {
            // initialize state.
            DateTime newLastProcessed = DateTime.Now;

            if (directories == null)
                throw new Exception("No files specified.");

            log("Processing " + directories.Count() + " directories.");

            foreach (string logDir in directories)
            {
                log("Processing directory " + logDir);
                foreach (string logFile in Directory.EnumerateFiles(logDir)) 
                {
                    // Checking only for newer files.
                    // Can't use the ProcessFile function below because that will update the last-processed.
                    var info = new FileInfo(logFile);
    
                    if (info.LastWriteTime > lastProcessed)
                    {
                        log("Processing file " + logFile);
                        log(HsParser.uploadLog(logFile));
                    }
                }
            }

            // Update our time.
            lastProcessed = newLastProcessed;
            log("Processing pass complete at " + lastProcessed);
        }


        #region Last Update Time
        private static readonly RegistryKey hive = Registry.CurrentUser;
        private const string regKey = "Software\\CasualAddict\\CAClient";
        private const string regValue = "LastProcessedTime";
        private void loadLastProcessedTime()
        {
            try {
                RegistryKey key = hive.OpenSubKey(regKey);
                string val = (string)key.GetValue(regValue);
                lastProcessed = DateTime.Parse(val);
            }
            catch (Exception)
            {
                lastProcessed = new DateTime(0);
            }
        }

        private void storeLastProcessedTime()
        {
            RegistryKey key = hive.OpenSubKey(regKey);
            if (key == null)
            {
                hive.CreateSubKey(regKey);
                key = hive.OpenSubKey(regKey);
            }

            try
            {
                key.SetValue(regValue, lastProcessed.ToString());
            }
            catch (Exception)
            {
                return;
            }
        }
        #endregion
    }
}
