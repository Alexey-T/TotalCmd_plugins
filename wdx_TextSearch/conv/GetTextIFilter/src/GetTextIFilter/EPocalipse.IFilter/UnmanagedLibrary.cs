using System;
using System.Runtime.ConstrainedExecution;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using Microsoft.Win32.SafeHandles;

namespace EPocalipse.IFilter
{
    /// <summary>
    /// Utility class to wrap an unmanaged DLL and be responsible for freeing it.
    /// </summary>
    /// <remarks>This is a managed wrapper over the native LoadLibrary, GetProcAddress, and
    /// FreeLibrary calls.
    /// </remarks>remarks>
    /// <example>
    /// Sample usage may be:
    ///<code>
    ///using(UnmanagedLibrary lib = new UnmanagedLibrary("kernel32")  // becomes call to LoadLibrary
    ///{ 
    ///   Action<String> function = lib.GetUnmanagedFunction<Action<String>>("DeleteFile"); // GetProcAddress
    ///   function(@"c:\tmp.txt");
    ///} // implict call to lib.Dispose, which calls FreeLibrary.
    /// </code>
    /// </example>
    public sealed class UnmanagedLibrary : IDisposable
    {
        #region Safe Handles and Native imports

        // See http://msdn.microsoft.com/msdnmag/issues/05/10/Reliability/ for more about safe handles.
        #region Nested type: NativeMethods

        private static class NativeMethods
        {
            private const string kernel = "kernel32";

            [DllImport(kernel, CharSet = CharSet.Auto, BestFitMapping = false, SetLastError = true)]
            public static extern SafeLibraryHandle LoadLibrary(string fileName);

            [ReliabilityContract(Consistency.WillNotCorruptState, Cer.Success)]
            [DllImport(kernel, SetLastError = true)]
            [return : MarshalAs(UnmanagedType.Bool)]
            public static extern bool FreeLibrary(IntPtr hModule);

            [DllImport(kernel)]
            public static extern IntPtr GetProcAddress(SafeLibraryHandle hModule, String procname);
        }

        #endregion
        #region Nested type: SafeLibraryHandle

        [SecurityPermission(SecurityAction.LinkDemand, UnmanagedCode = true)]
        public sealed class SafeLibraryHandle : SafeHandleZeroOrMinusOneIsInvalid
        {
            private SafeLibraryHandle() : base(true)
            {}

            protected override bool ReleaseHandle()
            {
                return NativeMethods.FreeLibrary(handle);
            }
        }

        #endregion

        #endregion // Safe Handles and Native imports
        private readonly SafeLibraryHandle _hLibrary;

        /// <summary>
        /// Constructor to load a dll and be responible for freeing it.
        /// </summary>
        /// <param name="fileName">full path name of dll to load</param>
        /// <remarks>Throws exceptions on failure. Most common failure would be file-not-found, or
        /// that the file is not a  loadable image.</remarks>
        public UnmanagedLibrary(string fileName)
        {
            _hLibrary = NativeMethods.LoadLibrary(fileName);
            if (HLibrary.IsInvalid)
            {
                int hr = Marshal.GetHRForLastWin32Error();
                Marshal.ThrowExceptionForHR(hr);
            }
        }

        public SafeLibraryHandle HLibrary
        {
            get { return _hLibrary; }
        }
        #region IDisposable Members

        /// <summary>
        /// Call FreeLibrary on the unmanaged dll. All function pointers
        /// handed out from this class become invalid after this.
        /// </summary>
        /// <remarks>This is very dangerous because it suddenly invalidate
        /// everything retrieved from this dll. This includes any functions
        /// handed out via GetProcAddress, and potentially any objects returned
        /// from those functions (which may have an implemention in the
        /// dll).
        /// </remarks>
        public void Dispose()
        {
            if (!HLibrary.IsClosed)
            {
                HLibrary.Close();
            }
        }

        #endregion
        /// <summary>
        /// Dynamically lookup a function in the dll via kernel32!GetProcAddress.
        /// </summary>
        /// <param name="functionName">raw name of the function in the export table.</param>
        /// <returns>null if function is not found. Else a delegate to the unmanaged function.
        /// </returns>
        /// <remarks>GetProcAddress results are valid as long as the dll is not yet unloaded. This
        /// is very very dangerous to use since you need to ensure that the dll is not unloaded
        /// until after you're done with any objects implemented by the dll. For example, if you
        /// get a delegate that then gets an IUnknown implemented by this dll,
        /// you can not dispose this library until that IUnknown is collected. Else, you may free
        /// the library and then the CLR may call release on that IUnknown and it will crash.</remarks>
        public TDelegate GetUnmanagedFunction<TDelegate>(string functionName) where TDelegate : class
        {
            IntPtr ptr = NativeMethods.GetProcAddress(HLibrary, functionName);

            // Failure is a common case, especially for adaptive code.
            if (ptr == IntPtr.Zero)
            {
                return null;
            }
            Delegate function = Marshal.GetDelegateForFunctionPointer(ptr, typeof (TDelegate));

            //return Marshal.GetDelegateForFunctionPointer(ptr, typeof(TDelegate)) as TDelegate;

            // Ideally, we'd just make the constraint on TDelegate be
            // System.Delegate, but compiler error CS0702 (constrained can't be System.Delegate)
            // prevents that. So we make the constraint system.object and do the cast from object-->TDelegate.
            object obj = function;

            return /*(TDelegate)*/ obj as TDelegate;
        }

        // Unmanaged resource. CLR will ensure SafeHandles get freed, without requiring a finalizer on this class.
    } // UnmanagedLibrary
}