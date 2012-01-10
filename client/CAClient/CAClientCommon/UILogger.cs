using System;
using System.Text;
using System.Windows.Forms;

namespace CAClientCommon
{
    public class UILogger
    {
        public TextBox logLabel;

        private StringBuilder logData;

        public UILogger(TextBox output)
        {
            logLabel = output;
            logData = new StringBuilder();
        }

        delegate void UpdateLabelDelegate(string msg);

        public void log(string msg) {
            if (logLabel.InvokeRequired)
            {
                var d = new UpdateLabelDelegate(log);
                logLabel.Invoke(d, new object[] { msg });
            } else {
                logData.Append(msg.Replace("\n", "\r\n") + "\r\n");
                logLabel.Text = logData.ToString();
                logLabel.Select(logLabel.Text.Length, logLabel.Text.Length);
                logLabel.ScrollToCaret();
            }
        }
    }
}
