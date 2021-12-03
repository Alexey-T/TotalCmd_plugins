using System;
using System.Collections.Generic;

namespace GetTextIFilter.Utils
{
    /// <summary>
    /// The GetOpt class allows you to easily define and handle
    /// command line options and arguments. Various styles of
    /// options are catered for each optionally having its own
    /// argument. For example: long options: (--foo), long options
    /// with arguments (--foo=bar), short options (-f, /f), short
    /// options with arguments (-f bar, -fbar) and consolidated
    /// short options (-fb).
    /// </summary>
    /// <example>
    /// This example defines 4 options (a, b, foo and bar). "foo"
    /// is required to be given with an argument (eg. --foo=xyz)
    /// and bar can be supplied with or without an argument.
    /// <code>
    /// try {
    ///		GetOpt foo = new GetOpt(args);
    ///		GetOpt.SetOpts(new string[] {"a", "b", "foo=", "bar=?"})
    ///		GetOpt.Parse();
    ///	} catch (ArgumentException e) {
    ///		myProgram.PrintUsage();
    ///	}
    /// </code>
    /// </example>
    public class GetOpt
    {
        #region Internal structure

        /// <summary>
        /// The CommandlineOption struct is used for representing
        /// a single option, with or without its argument.
        /// </summary>
        internal struct CommandlineOption
        {
            internal string arg;
            internal bool argOptional;
            internal bool argPresent;
            internal bool argRequired;
            internal string option;
        }

        #endregion
        #region Class fields

        private string[] args;
        private List<string> givenArgs;
        private Dictionary<string, CommandlineOption> givenOpts;
        private Dictionary<string, CommandlineOption> longOpts;
        private Dictionary<string, CommandlineOption> shortOpts;
        private bool parsed;

        #endregion
        #region Class properties

        /// <summary>
        /// Returns an ArrayList of supplied arguments (not options).
        /// If no arguments are supplied (ie none option args) then this
        /// will be empty.
        /// </summary>
        public List<string> Args
        {
            get { return givenArgs; }
        }

        #endregion
        #region Class methods

        /// <summary>
        /// Constructor for the GetOpt class. Pass it the programs args
        /// list.
        /// </summary>
        /// <param name="args">The args as supplied to the Main() method.</param>
        public GetOpt(string[] args)
        {
            this.args = args;
            parsed = false;

            givenArgs = new List<string>();
            givenOpts = new Dictionary<string, CommandlineOption>();
            shortOpts = new Dictionary<string, CommandlineOption>();
            longOpts = new Dictionary<string, CommandlineOption>();
        }

        /// <summary>
        /// Defines the options you require for your program. You should
        /// pass if the options as a string array. There are two modifiers
        /// you can use to specify option arguments. Use "=" at the end of
        /// your option to specify that this option has to have an argument,
        /// and use =? at the end of your option to specify that an argument
        /// is optional.
        /// </summary>
        /// <example>
        /// <code>
        /// GetOpt.SetOpts(new string[] {"a", "b", "foo=", "bar=?"})
        /// </code>
        /// </example>
        /// <param name="options">
        /// A string  array of the options without the preceding
        /// "-", "--" or "/".
        /// </param>
        public void SetOpts(string[] options)
        {
            foreach (string option in options)
            {
                CommandlineOption optionStruct = new CommandlineOption();

                // Argument required
                if (option.EndsWith("="))
                {
                    optionStruct.option = option.Substring(0, option.Length - 1);
                    optionStruct.argRequired = true;

                    // Argument optional
                }
                else if (option.EndsWith("=?"))
                {
                    optionStruct.option = option.Substring(0, option.Length - 2);
                    optionStruct.argOptional = true;

                    // No argument
                }
                else
                {
                    optionStruct.option = option;
                }

                if (optionStruct.option.Length == 1)
                {
                    shortOpts.Add(optionStruct.option, optionStruct);
                }
                else
                {
                    longOpts.Add(optionStruct.option, optionStruct);
                }
            }
        }

        /// <summary>
        /// Initiates the parsing of the options/arguments supplied
        /// to the program.
        /// </summary>
        /// <exception cref="System.ArgumentException">
        /// Thrown when: 1) an option is supplied with an argument and
        /// it shouldn't be, 2) an option is required to have an argument
        /// and it isn't supplied with one and 3) an option is supplied
        /// that has not been defined.
        /// </exception>
        public void Parse()
        {
            parsed = true;

            for (int i = 0; i < args.Length; ++i)
            {
                CommandlineOption optionStruct = new CommandlineOption();

                string arg = args[i];

                // Long option
                if (arg.StartsWith("--"))
                {
                    // Option has an argument (eg --foo=bar)
                    int idx;
                    if ((idx = arg.IndexOf("=")) != -1)
                    {
                        optionStruct.option = arg.Substring(2, idx - 2);
                        optionStruct.arg = arg.Substring(idx + 1);
                        optionStruct.argPresent = true;

                        // Option doesn't have an argument
                    }
                    else
                    {
                        optionStruct.option = arg.Substring(2);
                    }

                    // Checks
                    if (!longOpts.ContainsKey(optionStruct.option))
                    {
                        throw new ArgumentException("Unknown option specified", optionStruct.option);
                    }
                    else if (longOpts[optionStruct.option].argRequired &&
                             optionStruct.argPresent == false)
                    {
                        throw new ArgumentException("Option requires an argument", optionStruct.option);
                    }
                    else if (longOpts[optionStruct.option].argOptional == false
                             && longOpts[optionStruct.option].argRequired == false
                             && optionStruct.argPresent)
                    {
                        throw new ArgumentException("No argument permitted for this option", optionStruct.option);
                    }

                    // Add to defined options hashtable
                    givenOpts.Add(optionStruct.option, optionStruct);

                    // Short option(s)
                }
                else if (arg.StartsWith("-") || arg.StartsWith("/"))
                {
                    optionStruct.option = arg.Substring(1, 1);

                    // Is this option defined?
                    if (shortOpts.ContainsKey(optionStruct.option))
                    {
                        CommandlineOption definedOption = shortOpts[optionStruct.option];

                        // Check for arguments to the option
                        if (definedOption.argOptional || definedOption.argRequired)
                        {
                            if (arg.Length > 2)
                            {
                                optionStruct.argPresent = true;
                                optionStruct.arg = arg.Substring(2);
                            }
                            else if ((i + 1) < args.Length)
                            {
                                optionStruct.argPresent = true;
                                optionStruct.arg = args[++i];

                                // No option, but one was required
                            }
                            else if (definedOption.argRequired)
                            {
                                throw new ArgumentException("Option requires an argument", optionStruct.option);
                            }

                            // No argument optional or required, so if arg is more
                            // than just one letter, it could be multiple consolidated
                            // options.
                        }
                        else if (arg.Length > 2)
                        {
                            args[i] = "-" + arg.Substring(2);
                            --i;
                        }

                        // Add to defined options hashtable
                        givenOpts.Add(optionStruct.option, optionStruct);
                    }
                    else
                    {
                        throw new ArgumentException("Unknown option specified", optionStruct.option);
                    }


                    // Not an option, an argument
                }
                else
                {
                    givenArgs.Add(arg);
                }
            }
        }

        /// <summary>
        /// Returns true/false as to whether an option has been supplied.
        /// </summary>
        /// <param name="option">The option in question</param>
        /// <returns>Whether the option was given</returns>
        /// <exception cref="System.Exception">
        /// Thrown when the Parse() method has not been called.
        /// </exception>
        public bool IsDefined(string option)
        {
            if (parsed)
            {
                return givenOpts.ContainsKey(option);
            }
            else
            {
                throw new Exception("Call the Parse() method first");
            }
        }

        /// <summary>
        /// Returns true/false based on whether the given option has
        /// an argument or not.
        /// </summary>
        /// <param name="option">The option to check</param>
        /// <returns>Whether the option has an argument</returns>
        /// <example>
        /// <code>
        /// if (optionParser.IsDefined("bar") &amp;&amp; optionParser.HasArgument("bar")) {
        ///		...
        /// }
        /// </code>
        /// </example>
        /// <exception cref="System.Exception">
        /// Thrown when the given option is not defined
        /// </exception>
        public bool HasArgument(string option)
        {
            if (IsDefined(option))
            {
                return givenOpts[option].argPresent;
            }
            else
            {
                throw new Exception("Option ({0}) is not defined");
            }
        }

        /// <summary>
        /// Returns the supplied options argument. Throws an exception
        /// if option does not have one.
        /// </summary>
        /// <param name="option">The option you want the argument for</param>
        /// <returns>The options argument</returns>
        /// <example>
        /// <code>
        /// if (optionParser.IsDefined("bar") &amp;&amp; optionParser.HasArgument("bar")) {
        ///		string optionArg = optionParser.GetOptionArg("bar");
        /// }
        /// </code>
        /// </example>
        /// <exception cref="System.Exception">
        /// Thrown when the supplied option does not have an argument
        /// </exception>
        public string GetOptionArg(string option)
        {
            if (HasArgument(option))
            {
                return givenOpts[option].arg;
            }
            else
            {
                throw new Exception("Option does not have an argument");
            }
        }

        #endregion
    }
}