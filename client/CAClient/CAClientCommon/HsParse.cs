using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;


namespace CAClientCommon
{
    public static class HsParser
    {
        [DllImport("Parser.dll", EntryPoint="HsStart")]
        public static extern void HsStart();

        [DllImport("Parser.dll", EntryPoint="HsEnd")]
        public static extern void HsEnd();

        [DllImport("Parser.dll", EntryPoint="cGamesAsJSON@4")]
        public static extern IntPtr cGamesAsJson(StringBuilder input);

        [DllImport("Parser.dll", EntryPoint="parseFile@4")]
        public static extern IntPtr cParseFile(StringBuilder input);

        [DllImport("Parser.dll", EntryPoint="uploadLog@4")]
        public static extern IntPtr cUploadLog(StringBuilder input);

        private static bool rtsStarted = false;
        private static void EnsureRtsStarted() {
            if (!rtsStarted)
            {
                HsStart();
                rtsStarted = true;
            }
        }

        public static string parseString(string input) {
            EnsureRtsStarted();
            return Marshal.PtrToStringAnsi(cGamesAsJson(new StringBuilder(input)));
        }
        public static string parseFile(string input) {
            EnsureRtsStarted();
            return Marshal.PtrToStringAnsi(cParseFile(new StringBuilder(input)));
        }

        public static string uploadLog(string input)
        {
            EnsureRtsStarted();
            return Marshal.PtrToStringAnsi(cUploadLog(new StringBuilder(input)));
        }
    }
}
