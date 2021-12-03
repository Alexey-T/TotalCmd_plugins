using System;
using System.IO;
using System.Runtime.InteropServices;
using EPocalipse.IFilter;
using GetTextIFilter.Utils;

namespace GetTextIFilter
{
    internal class Program
    {
        private static readonly string usage =
            String.Format(
                "Usage:{0}GetTextIFilter /(-)s <full path to source file> /(-)d <path to destination file> [/(-)l enable errors log] [/(-)? | /(-)h | --help print usage]",
                Environment.NewLine);

        private static bool log = false;

        private static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.Write(usage);
                return;
            }
            else
            {
                string src = String.Empty;
                string dst = String.Empty;
                try
                {
                    GetOpt opt = new GetOpt(args);
                    opt.SetOpts(new string[] {"s=", "d=", "l", "?", "h", "help"});
                    opt.Parse();

                    if (opt.IsDefined("s") && opt.HasArgument("s"))
                        src = opt.GetOptionArg("s");

                    if (opt.IsDefined("d") && opt.HasArgument("d"))
                        dst = opt.GetOptionArg("d");

                    if (opt.IsDefined("l"))
                        log = true;

                    if (opt.IsDefined("?") || opt.IsDefined("h") || opt.IsDefined("help"))
                    {
                        Console.Write(usage);
                        return;
                    }
                }
                catch (Exception ex)
                {
#if DEBUG
                    WriteError(ex.ToString());
#else
                    WriteError(ex.Message);
#endif
                    return;
                }

                if (!File.Exists(src))
                {
                    WriteError("Source file '{0}' does not exists!", src);
                    return;
                }

                if (!IsFileNameValid(dst))
                {
                    WriteError("Invalid destination filename: '{0}'", dst);
                    return;
                }

                string plainText = GetText(src);

                using (FileStream fs = new FileStream(dst, FileMode.Create))
                using (StreamWriter sw = new StreamWriter(fs, EncodingTools.GetMostEfficientEncoding(plainText)))
                {
                    sw.Write(plainText);
                    sw.Flush();
                    sw.Close();
                }
            }
        }

        private static void WriteError(string message, params Object[] args)
        {
            if (log)
            {
                Log.Error(String.Format(message, args));
            }
        }

        private static string GetText(string srcPath)
        {
            TextReader reader = null;
            try
            {
                using (reader = new FilterReader(srcPath))
                {
                    return reader.ReadToEnd();
                }
            }
            catch (COMException ex)
            {
#if DEBUG
                WriteError(ex.ToString());
#else
                WriteError(ex.Message);
#endif
                return String.Empty;
            }
            catch (ArgumentException ex)
            {
#if DEBUG
                WriteError(ex.ToString());
#else
                WriteError(ex.Message);
#endif
                return String.Empty;
            }
            catch (Exception ex)
            {
#if DEBUG
                WriteError(ex.ToString());
#else
                WriteError(ex.Message);
#endif
                return String.Empty;
            }
            finally
            {
                if (reader != null)
                {
                    reader.Close();
                    reader.Dispose();
                }
            }
        }

        private static bool IsFileNameValid(string fileName)
        {
            if (String.IsNullOrEmpty(fileName) || fileName.Length >= 260)
            {
                return false;
            }

            foreach (char invalidChar in Path.GetInvalidPathChars())
            {
                if (fileName.IndexOf(invalidChar) >= 0)
                {
                    return false;
                }
            }

            string nameWithoutExtension = Path.GetFileNameWithoutExtension(fileName);
            if (nameWithoutExtension != null)
            {
                nameWithoutExtension = nameWithoutExtension.ToUpper();
            }

            if (nameWithoutExtension == "CON" ||
                nameWithoutExtension == "PRN" ||
                nameWithoutExtension == "AUX" ||
                nameWithoutExtension == "NUL")
            {
                return false;
            }

            if (nameWithoutExtension != null)
            {
                char ch = nameWithoutExtension.Length == 4 ? nameWithoutExtension[3] : '\0';

                return !((nameWithoutExtension.StartsWith("COM") ||
                          nameWithoutExtension.StartsWith("LPT")) &&
                         Char.IsDigit(ch));
            }
            return false;
        }
    }
}