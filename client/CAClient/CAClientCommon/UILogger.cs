using System;
using System.Text;
using System.Windows.Forms;

namespace CAClientCommon
{
    public class UILogger
    {
        public Control logLabel;

        public UILogger(Control output)
        {
            logLabel = output;
        }

        public void log(string msg) {
            lock (this)
            {
                logLabel.Text = logLabel.Text + msg.Replace("\n", "\r\n") + "\r\n";
            }
        }
    }
}
