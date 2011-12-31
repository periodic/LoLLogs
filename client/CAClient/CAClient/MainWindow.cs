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
        private static RegistryKey caKey = Registry.CurrentUser.OpenSubKey("\\Software\\CasualAddict\\CAClient", true);
        private static string registryValueName = "LoLFolder";
        private UILogger logger;

        private void log(string msg)
        {
            logger.log(msg);
        }

        public MainWindow()
        {

            InitializeComponent();

            logger = new UILogger(logText);

            var files = new List<string>();
            DirFinder[] dirFinders = { new RegistryDirFinder(Registry.CurrentUser.OpenSubKey("\\Software\\Riot Games\\RADS"), "LocalRootFolder")
                                     , new RegistryDirFinder(caKey, registryValueName)
                                     , new StaticDirFinder("C:\\Riot Games\\League of Legends") 
                                     , new DialogDirFinder()
                                     };

            var dirSrcs = new List<DirFinder>(dirFinders);

            foreach (DirFinder dir in dirSrcs)
            {
                try
                {
                    files.AddRange(dir.getFiles());
                    saveDirInRegistry(dir.getDirectory());
                    break;
                }
                catch (Exception e)
                {
                    log(e.Message + e.StackTrace);
                }
            }

            //files.Add("D:\\Games\\League of Legends\\RADS\\projects\\lol_air_client\\releases\\0.0.0.116\\deploy\\logs\\LolClient.20111227.215719.log");

            if (files.Count() == 0) 
            {
                log("No files found to process.");
            }
            else
            {
                log("Starting processing.");
                progressBar.Maximum = files.Count();
    
                UploadWorker worker = new UploadWorker(files);
                backgroundUploader.RunWorkerAsync(worker);
            }

        }

        private void saveDirInRegistry(string dir)
        {
            caKey.SetValue(registryValueName, dir);
            log("Saved directory in the registry for future use.");
        }

        private void quitButton_Click(object sender, EventArgs e)
        {
            System.Environment.Exit(0);
        }

        private void backgroundWorker1_DoWork(object sender, DoWorkEventArgs e)
        {
            var worker = (System.ComponentModel.BackgroundWorker)sender;

            UploadWorker uploader = (UploadWorker)e.Argument;

            uploader.ProcessFiles(worker, e);
        }

        private void backgroundWorker1_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            try
            {
                UploadWorker.CurrentState state = (UploadWorker.CurrentState)e.UserState;
                progressBar.Value = state.filesProcessed;
            }
            catch (InvalidCastException e1)
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
        }

        private void backgroundWorker1_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            if (e.Error != null)
                log("Error processing files: " + e.Error.Message);
            else if (e.Cancelled)
                log("Processing cancelled.");
            else
                log("Processing complete.");
        }

        public class UploadWorker
        {
            public class CurrentState
            {
                public int filesProcessed;
                public int gamesFound;
            }

            private List<string> files;

            private void log(BackgroundWorker worker, string msg)
            {
                worker.ReportProgress(0, msg);
            }

            public UploadWorker(List<string> files) {
                this.files = files;
            }

            public void ProcessFiles(BackgroundWorker worker, DoWorkEventArgs e)
            {
                log(worker, "Processing started.");
                // initialize state.
                CurrentState state = new CurrentState();
                DateTime lastReportDateTime = DateTime.Now;

                if (files == null)
                    throw new Exception("No files specified.");

                // Parser parser = new Parser();
                // var parser = new LogParserFromString();

                log(worker, "Processing " + files.Count() + " files.");
                foreach (string logFile in files)
                {
                    // Cancel if requested.
                    if (worker.CancellationPending)
                    {
                        log(worker, "Processing cancelled.");
                        e.Cancel = true;
                        break;
                    }


                    //string data = File.OpenText(logFile).ReadToEnd();
                    log(worker, HsParser.uploadLog(logFile));

                    /*
                    ParserCombinators.Result<string, DList<LogParsers<string>.LogMsg>> parsedData = parser.LogMsgs(data, 0);
                    */

                    log(worker, "Processing " + logFile);

                    state.filesProcessed += 1;
                    worker.ReportProgress(0, state);

                    /*
                    if (parsedData.Value.Count() > 0)
                    {
                        log(worker, "Found " + parsedData.Value.Count() + " messages."); ;
                        foreach (LogParsers<string>.LogMsg msg in parsedData.Value)
                        {
                            Value data = msg.data;
                            if (data != null)
                            {
                                log(worker, "Found game."); ;
                                state.gamesFound += 1;
                                worker.ReportProgress(0, state);
                            }

                        }
                    }
                    */
                }
                log(worker, "Processing complete.");
            }
        }


        private void detailsButton_Click(object sender, EventArgs e)
        {
            if (logText.Visible)
            {
                logText.Hide();
            }
            else
            {
                logText.Show();
            }
        }

    }
}
