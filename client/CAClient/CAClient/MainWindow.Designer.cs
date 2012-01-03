namespace CAClient
{
    partial class MainWindow
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.progressBar = new System.Windows.Forms.ProgressBar();
            this.quitButton = new System.Windows.Forms.Button();
            this.Title = new System.Windows.Forms.Label();
            this.logText = new System.Windows.Forms.TextBox();
            this.backgroundUploader = new System.ComponentModel.BackgroundWorker();
            this.detailsButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // progressBar
            // 
            this.progressBar.Location = new System.Drawing.Point(12, 25);
            this.progressBar.Maximum = 1;
            this.progressBar.Name = "progressBar";
            this.progressBar.Size = new System.Drawing.Size(410, 23);
            this.progressBar.TabIndex = 0;
            // 
            // quitButton
            // 
            this.quitButton.Location = new System.Drawing.Point(347, 54);
            this.quitButton.Name = "quitButton";
            this.quitButton.Size = new System.Drawing.Size(75, 23);
            this.quitButton.TabIndex = 4;
            this.quitButton.Text = "Quit";
            this.quitButton.UseVisualStyleBackColor = true;
            this.quitButton.Click += new System.EventHandler(this.quitButton_Click);
            // 
            // Title
            // 
            this.Title.AutoSize = true;
            this.Title.Location = new System.Drawing.Point(12, 9);
            this.Title.Name = "Title";
            this.Title.Size = new System.Drawing.Size(100, 13);
            this.Title.TabIndex = 5;
            this.Title.Text = "Uploading Games...";
            // 
            // logText
            // 
            this.logText.AcceptsReturn = true;
            this.logText.Location = new System.Drawing.Point(12, 83);
            this.logText.Multiline = true;
            this.logText.Name = "logText";
            this.logText.ReadOnly = true;
            this.logText.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.logText.Size = new System.Drawing.Size(410, 300);
            this.logText.TabIndex = 6;
            this.logText.Visible = false;
            // 
            // backgroundUploader
            // 
            this.backgroundUploader.WorkerReportsProgress = true;
            this.backgroundUploader.WorkerSupportsCancellation = true;
            this.backgroundUploader.DoWork += new System.ComponentModel.DoWorkEventHandler(this.backgroundWorker1_DoWork);
            this.backgroundUploader.ProgressChanged += new System.ComponentModel.ProgressChangedEventHandler(this.backgroundWorker1_ProgressChanged);
            this.backgroundUploader.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(this.backgroundWorker1_RunWorkerCompleted);
            // 
            // detailsButton
            // 
            this.detailsButton.AutoSize = true;
            this.detailsButton.Location = new System.Drawing.Point(249, 54);
            this.detailsButton.Name = "detailsButton";
            this.detailsButton.Size = new System.Drawing.Size(92, 23);
            this.detailsButton.TabIndex = 7;
            this.detailsButton.Text = "Show Details";
            this.detailsButton.UseVisualStyleBackColor = true;
            this.detailsButton.Click += new System.EventHandler(this.detailsButton_Click);
            // 
            // MainWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.ClientSize = new System.Drawing.Size(434, 395);
            this.Controls.Add(this.detailsButton);
            this.Controls.Add(this.logText);
            this.Controls.Add(this.Title);
            this.Controls.Add(this.quitButton);
            this.Controls.Add(this.progressBar);
            this.Name = "MainWindow";
            this.Text = "CA Upload Client";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ProgressBar progressBar;
        private System.Windows.Forms.Button quitButton;
        private System.Windows.Forms.Label Title;
        private System.Windows.Forms.TextBox logText;
        private System.ComponentModel.BackgroundWorker backgroundUploader;
        private System.Windows.Forms.Button detailsButton;
    }
}

