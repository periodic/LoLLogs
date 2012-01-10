using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.IO;
using System.Windows.Forms;
using System.Threading;
using Microsoft.Win32;

using CAClientCommon;

namespace CAClient
{
    public partial class MainWindow : Form
    {
        private static readonly RegistryKey caHive = Registry.CurrentUser;
        private const string caKey = "Software\\CasualAddict\\CAClient";
        private const string caValue = "LoLFolder";
        public MainWindow()
        {

            InitializeComponent();
            InitializeNotifyIcon();
            InitializeLogger(logText);

            var logDirs = new List<string>();
            DirFinder[] dirFinders = { new RegistryDirFinder(Registry.CurrentUser, "Software\\Riot Games\\RADS", "LocalRootFolder")
                                     , new RegistryDirFinder(caHive, caKey, caValue)
                                     , new StaticDirFinder("C:\\Riot Games\\League of Legends") 
                                     , new DialogDirFinder()
                                     };

            var dirSrcs = new List<DirFinder>(dirFinders);

            foreach (DirFinder dir in dirSrcs)
            {
                try
                {
                    logDirs.AddRange(dir.getDirs());
                    saveDirInRegistry(dir.getDirectory());
                    break;
                }
                catch (Exception e)
                {
                    log(e.Message);
                }
            }

            if (logDirs.Count() == 0) 
            {
                log("No files found to process.");
                setTitle("Unable to find LoL directory.");
            }
            else
            {
                log("Starting processing.");

                //backgroundUploader.RunWorkerAsync(logDirs);

                uploader = new UploadWorker(logDirs, logger);
                workerDelegate = new WorkerDelegate(StartWorker);

                RunWorker();
            }
        }

        #region Background Worker
        private UploadWorker uploader;

        delegate void WorkerDelegate();
        WorkerDelegate workerDelegate;

        private void RunWorker()
        {
            workerDelegate.BeginInvoke(null, null);
        }
        private void StartWorker()
        {
            uploader.start();
        }

        /*
        private void backgroundWorker1_DoWork(object sender, DoWorkEventArgs e)
        {
            var worker = (System.ComponentModel.BackgroundWorker)sender;

            List<string> dirs = (List<string>)e.Argument;

            uploader = new UploadWorker(dirs, worker);
            uploader.start();
        }

        private void backgroundWorker1_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            try
            {
                string msg = (string)e.UserState;
                log(msg);
            } catch (InvalidCastException e2) {
                log("Got invalid state type from worker.");
                throw e2;
            }
        }

        private void backgroundWorker1_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            if (e.Error != null)
            {
                log("Error processing files: " + e.Error.Message);
                setTitle("Error uploading files.");
            }
            else if (e.Cancelled)
            {
                log("Processing cancelled.");
                setTitle("Cancelled.");
            }
            else
            {
                log("Processing complete.");
                setTitle("Done.");
            }
        }
        */

        private void runNowButton_Click(object sender, EventArgs e)
        {
            RunWorker();
        }
        #endregion

        #region Event Handlers
        /* 
         * Event Handlers
         */

        private void closeButton_Click(object sender, EventArgs e)
        {
            hideWindow();
        }

        protected override void OnLoad(EventArgs e)
        {
            // Hide because we'll use a system tray icon.
            Visible = false;
            ShowInTaskbar = false;

            base.OnLoad(e);
        }

        private void OnExit(object sender, EventArgs e)
        {
            Application.Exit();
        }


        private void notifyIcon_Click(object sender, MouseEventArgs e)
        {
            /*
            if (Visible == false)
                showWindow();
            else
                hideWindow();
            */
        }
        #endregion

        #region Notification Icon
        /* 
         * NotifyIcon setup
         */
        private void InitializeNotifyIcon()
        {
            trayMenu.MenuItems.Add("Show Log", (sender, args) => showWindow());
            trayMenu.MenuItems.Add("Upload Now", (sender, args) => RunWorker());
            trayMenu.MenuItems.Add("Exit", OnExit);
            notifyIcon.ContextMenu = trayMenu;
        }
        #endregion

        #region Logger
        /* 
         * Logger setup
         */
        private UILogger logger;

        private void InitializeLogger(TextBox output)
        {
            logger = new UILogger(output);
        }
        private void log(string msg)
        {
            logger.log(msg);
        }
        #endregion

        #region Helpers
        /*
         * Helpers
         */
        private void setTitle(string title)
        {
            Title.Text = title;
        }
        private void hideWindow() 
        {
            Visible = false;
            ShowInTaskbar = false;
        }

        private void showWindow()
        {
            Visible = true;
            ShowInTaskbar = true;
        }

        private void saveDirInRegistry(string dir)
        {
            RegistryKey key = caHive.OpenSubKey(caKey, true);
            if (key == null)
            {
                caHive.CreateSubKey(caKey);
                key = caHive.OpenSubKey(caKey, true);
            }

            key.SetValue(caValue, dir);

            log("Saved directory in the registry for future use.");
        }
        #endregion

    }

}
