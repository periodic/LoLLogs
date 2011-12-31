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

using CAClientCommon;
using ParserCombinators;

namespace CAClient
{
    public partial class MainWindow : Form
    {
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
            DirFinder[] dirFinders = { new RegistryDirFinder("HKEY_CURRENT_USER\\Software\\Riot Games\\RADS", "LocalRootFolder")
                                     //, new StaticDirFinder("C:\\Riot Games\\League of Legends") 
                                     , new StaticDirFinder("D:\\Games\\League of Legends") 
                                     //, new DialogDirFinder()
                                     };

            var dirSrcs = new List<DirFinder>(dirFinders);

            foreach (DirFinder dir in dirSrcs)
            {
                try
                {
                    files = dir.getFiles();
                    break;
                }
                catch (Exception e)
                {
                    log(e.Message);
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
                var parser = new LogParserFromString();

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

        private void ParserTests()
        {
            var parser = new ASParserFromString();

            var testStrings = new string[] { "\"foo\"", "123.456", "false", "true", "123", "(null)", "(null)#5", "NaN", "a date or something.", "(my.silly::Object)#5\n  foo = 1\n  bar = \"baz\"\n", "(Array)#0\n  [0] 1\n  [1] \"Foo\"", "(my.nested::Object)#0\n  foo = (Array)#1\n    [0] 1\n    [1] 2\n  bar = (internal::Object)#2\n    foo = 1\n    bar = 2" };

            foreach (string tstr in testStrings)
            {
                try
                {
                    ParserCombinators.Result<string, Value> result =
                        parser.Val(tstr, 0);

                    log(tstr + " => " + result.Value.ToString());
                }
                catch (Exception e)
                {
                    log(tstr + " => " + e.Message + e.StackTrace);
                }
            }

            var logParser = new LogParserFromString();

            testStrings = new string[] {
"12/22/2011 23:19:06.375 [INFO] com.riotgames.platform.common.utils.LogManager RiotApplication: initializing\n" +
"12/22/2011 23:47:05.796 [DEBUG] com.riotgames.platform.gameclient.module.services.RemoteObjectGenerator Got async message: (mx.messaging.messages::AsyncMessageExt)#0\n" +
"  body = (com.riotgames.platform.gameclient.domain::EndOfGameStats)#1\n" +
"    basePoints = 12\n" +
"    boostIpEarned = 0\n" +
"12/22/2011 23:19:06.375 [DEBUG] com.riotgames.platform.common.utils.LogManager This is a debug message.\n"
            };

            foreach (string tstr in testStrings)
            {
                try
                {
                    ParserCombinators.Result<string, DList<LogParsers<string>.LogMsg>> result =
                        logParser.LogMsgs(tstr, 0);

                    string output = "[";
                    bool first = true;
                    foreach (var msg in result.Value)
                    {
                        if (first)
                            first = false;
                        else
                            output += ",";

                        output += msg.ToString();
                    }
                    output += "]";
                    log(tstr + " => " + output);
                }
                catch (Exception e)
                {
                    log(tstr + " => " + e.Message + e.StackTrace);
                }
            }
        }

    }


    public class ASParserFromString : ASParsers<string>
    {
        public override ParserCombinators.Parser<string, char> AnyChar
        {
            get
            {
                {
                    return (input, indent) => input.Length > 0 ? new ParserCombinators.Result<string, char>(input[0], input.Substring(1)) : null;
                }
            }
        }
    }
    public class LogParserFromString : LogParsers<string>
    {
        public override ParserCombinators.Parser<string, char> AnyChar
        {
            get
            {
                {
                    return (input, indent) => input.Length > 0 ? new ParserCombinators.Result<string, char>(input[0], input.Substring(1)) : null;
                }
            }
        }
    }
    public class LogParserFromStream : LogParsers<StreamReader>
    {
        public override ParserCombinators.Parser<StreamReader, char> AnyChar
        {
            get
            {
                {
                    return (input, indent) => input.EndOfStream ? new ParserCombinators.Result<StreamReader, char>((char)input.Read(), input) : null;
                }
            }
        }
    }
}
