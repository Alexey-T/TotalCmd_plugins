using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace EPocalipse.IFilter
{
    [ComVisible(false)]
    [ComImport]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    [Guid("00000001-0000-0000-C000-000000000046")]
    internal interface IClassFactory
    {
        void CreateInstance([MarshalAs(UnmanagedType.Interface)] object pUnkOuter,
                            ref Guid refiid,
                            [MarshalAs(UnmanagedType.Interface)] out object ppunk);

        void LockServer(bool fLock);
    }

    /// <summary>
    /// Utility class to get a Class Factory for a certain Class ID 
    /// by loading the dll that implements that class
    /// </summary>
    internal static class ComHelper
    {
        //DllGetClassObject fuction pointer signature

        private static DllList _dllList = new DllList();

        /// <summary>
        /// Gets a class factory for a specific COM Class ID. 
        /// </summary>
        /// <param name="dllName">The dll where the COM class is implemented</param>
        /// <param name="filterPersistClass">The requested Class ID</param>
        /// <returns>IClassFactory instance used to create instances of that class</returns>
        internal static IClassFactory GetClassFactory(string dllName, string filterPersistClass)
        {
            //Load the class factory from the dll
            IClassFactory classFactory = GetClassFactoryFromDll(dllName, filterPersistClass);
            return classFactory;
        }

        //TODO Make Safe Handles with UnmanagedLibrary !!!
        private static IClassFactory GetClassFactoryFromDll(string dllName, string filterPersistClass)
        {
            //Load the dll
            IntPtr dllHandle = Win32NativeMethods.LoadLibrary(dllName);
            if (dllHandle == IntPtr.Zero)
            {
                throw new Win32Exception(Marshal.GetLastWin32Error());
                //return null;
            }


            //Keep a reference to the dll until the process\AppDomain dies
            _dllList.AddDllHandle(dllHandle);

            //Get a pointer to the DllGetClassObject function
            IntPtr dllGetClassObjectPtr = Win32NativeMethods.GetProcAddress(dllHandle, "DllGetClassObject");
            if (dllGetClassObjectPtr == IntPtr.Zero)
            {
                throw new Win32Exception(Marshal.GetLastWin32Error());
                //return null;
            }

            //Convert the function pointer to a .net delegate
            DllGetClassObject dllGetClassObject =
                (DllGetClassObject)
                Marshal.GetDelegateForFunctionPointer(dllGetClassObjectPtr, typeof (DllGetClassObject));

            //Call the DllGetClassObject to retreive a class factory for out Filter class
            Guid filterPersistGUID = new Guid(filterPersistClass);
            Guid IClassFactoryGUID = new Guid("00000001-0000-0000-C000-000000000046"); //IClassFactory class id
            Object unk;
            if (dllGetClassObject(ref filterPersistGUID, ref IClassFactoryGUID, out unk) != 0)
                return null;

            //UnmanagedLibrary lib = new UnmanagedLibrary(dllName);
            //DllGetClassObject function = lib.GetUnmanagedFunction<DllGetClassObject>("DllGetClassObject");
            //if (function(ref filterPersistGUID, ref IClassFactoryGUID, out unk) != 0)
            //    return null;

            return (unk as IClassFactory);
        }
        #region Nested type: DllGetClassObject

        private delegate int DllGetClassObject(
            ref Guid ClassId, ref Guid InterfaceId, [Out] [MarshalAs(UnmanagedType.Interface)] out object ppunk);

        #endregion
        #region Nested type: DllList

        /// <summary>
        /// Holds a list of dll handles and unloads the dlls 
        /// in the destructor
        /// </summary>
        private class DllList
        {
            private List<IntPtr> _dllList = new List<IntPtr>();

            public void AddDllHandle(IntPtr dllHandle)
            {
                lock (_dllList)
                {
                    _dllList.Add(dllHandle);
                }
            }

            ~DllList()
            {
                foreach (IntPtr dllHandle in _dllList)
                {
                    try
                    {
                        Win32NativeMethods.FreeLibrary(dllHandle);
                    }
                    catch
                    {}
                }
            }
        }

        #endregion
        #region Nested type: Win32NativeMethods

        private class Win32NativeMethods
        {
            [DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError = true)]
            public static extern IntPtr GetProcAddress(IntPtr hModule, string lpProcName);

            [DllImport("kernel32.dll", CharSet = CharSet.Auto, BestFitMapping = false, SetLastError = true)]
            public static extern bool FreeLibrary(IntPtr hModule);

            [DllImport("kernel32.dll", SetLastError = true)]
            public static extern IntPtr LoadLibrary(string lpFileName);
        }

        #endregion
    }
}