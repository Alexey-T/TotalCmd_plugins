using System;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace GetTextIFilter.Utils
{
    /// <summary>
    /// CustomLogHandler delegate.
    /// </summary>
    /// <param name="sender">object sender</param>
    /// <param name="e"><c>CustomLogEventArgs</c> object </param>
    public delegate void CustomLogHandler(object sender, Log.CustomLogEventArgs e);

    /// <summary>
    /// Application logging.
    /// </summary>
    public class Log
    {
        /// <summary>
        /// Is Debug logging enabled?
        /// </summary>
#if DEBUG
        public static bool DebugEnabled = true;
#else
        public static bool DebugEnabled = false;
#endif

        /// <summary>
        /// File path for storing debug messages
        /// </summary>
        public static string debugLogPath = Directory.GetParent(Assembly.GetExecutingAssembly().Location) + "/debug.txt";

        /// <summary>
        /// Is Error logging enabled?
        /// </summary>
        public static bool ErrorEnabled = true;

        /// <summary>
        /// File path for storing error messages
        /// </summary>
        public static string errorLogPath = Directory.GetParent(Assembly.GetExecutingAssembly().Location) + "/error.txt";

        /// <summary>
        /// CustomLog event.
        /// </summary>
        public static event CustomLogHandler CustomLog;

        /// <summary>
        /// Debug message.
        ///     Method can be executed by only one thread at a time. 
        ///     Static methods lock on the type, while instance methods lock on the instance. 
        ///     Only one thread can execute in any of the instance functions and only one 
        ///     thread can execute in any of a class's static functions. 
        /// </summary>
        /// <param name="obj">The message object.</param>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public static void Echo(object obj)
        {
            if (obj == null)
                return;

            string msg = obj.ToString();
            if (CustomLog != null)
                CustomLog(null, new CustomLogEventArgs(msg, false));

            if (debugLogPath == null)
                return;

            if (!DebugEnabled)
                return;

            using (StreamWriter sw = new StreamWriter(debugLogPath, true))
            {
                sw.WriteLine(DateTime.Now + " " + msg);
            }
        }

        /// <summary>
        /// Error message.
        ///     Method can be executed by only one thread at a time. 
        ///     Static methods lock on the type, while instance methods lock on the instance. 
        ///     Only one thread can execute in any of the instance functions and only one 
        ///     thread can execute in any of a class's static functions.
        /// </summary>
        /// <param name="obj">The message object.</param>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public static void Error(object obj)
        {
            if (obj == null)
                return;

            string msg = obj.ToString();
            if (CustomLog != null)
                CustomLog(null, new CustomLogEventArgs(msg, true));

            if (errorLogPath == null)
                return;

            if (!ErrorEnabled)
                return;

            using (StreamWriter sw = new StreamWriter(errorLogPath, true))
            {
                sw.WriteLine(DateTime.Now + " " + msg);
            }
        }
        #region class CustomLogEventArgs

        /// <summary>
        /// Class for event's data retrieving. 
        /// </summary>
        public class CustomLogEventArgs : EventArgs
        {
            private readonly bool bIsError = false;
            private readonly string strMessage = String.Empty;

            /// <summary>
            /// Initializes a new instance of the <see cref="CustomLogEventArgs"/> class.
            /// </summary>
            /// <param name="message">The event's message.</param>
            /// <param name="isError">if set to <c>true</c> current message is error.</param>
            public CustomLogEventArgs(string message, bool isError)
            {
                strMessage = message;
                bIsError = isError;
            }

            /// <summary>
            /// Gets the event's message.
            /// </summary>
            /// <value>The message.</value>
            public string Message
            {
                get { return strMessage; }
            }

            /// <summary>
            /// Gets a kind of message.
            /// </summary>
            /// <value><c>true</c> if this instance is error; otherwise, <c>false</c>.</value>
            public bool IsError
            {
                get { return bIsError; }
            }
        }

        #endregion
    }
}