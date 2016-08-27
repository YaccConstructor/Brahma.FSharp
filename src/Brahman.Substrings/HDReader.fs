module Brahman.Substrings.RawIO

open System;
open System.Runtime.InteropServices
open System.Threading;

[<Flags>]
type AccessRights =
    | GENERIC_READ = 0x80000000u
    | GENERIC_WRITE = 0x40000000u
    | GENERIC_EXECUTE = 0x20000000u
    | GENERIC_ALL = 0x10000000u

[<Flags>]
type ShareModes =
    | FILE_SHARE_READ = 0x00000001
    | FILE_SHARE_WRITE = 0x00000002
    | FILE_SHARE_DELETE = 0x00000004

type CreationDispositions =
    | CREATE_NEW = 1
    | CREATE_ALWAYS = 2
    | OPEN_EXISTING = 3
    | OPEN_ALWAYS = 4
    | TRUNCATE_EXISTING = 5

[<DllImport("kernel32.dll", SetLastError = true)>]
extern bool CloseHandle(IntPtr handle)

[<DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Auto)>]
extern IntPtr CreateFile(string lpFileName,
                                uint32 dwDesiredAccess,
                                uint32 dwShareMode,
                                IntPtr lpSecurityAttributes,
                                uint32 dwCreationDisposition,
                                int dwFlagsAndAttributes,
                                IntPtr hTemplateFile)

[<DllImport("kernel32.dll", SetLastError = true)>]
extern bool ReadFile(IntPtr hFile,                        // handle to file
                            byte[] lpBuffer,              // data buffer
                            int nNumberOfBytesToRead,     // number of bytes to read
                            int& lpNumberOfBytesRead,     // number of bytes read
                            IntPtr lpOverlapped)

[<DllImport("kernel32.dll", SetLastError = true)>]
extern int GetFileSize(IntPtr hFile,
                                int& high)

let CreateFileW(id:int) =    
    CreateFile(
        sprintf "\\\\.\\PhysicalDrive%i" id,
        Convert.ToUInt32(AccessRights.GENERIC_READ),
        Convert.ToUInt32(ShareModes.FILE_SHARE_READ ||| ShareModes.FILE_SHARE_WRITE),
        IntPtr.Zero,
        Convert.ToUInt32(CreationDispositions.OPEN_EXISTING),
        0,
        IntPtr.Zero)    

let ReadFileW(hFile, lpBuffer, nNumberOfBytesToRead, offset: int64)=
    let mutable overlapped = new NativeOverlapped()
    let bytes = BitConverter.GetBytes(offset)

    if (BitConverter.IsLittleEndian)
    then
        overlapped.OffsetHigh <- BitConverter.ToInt32(bytes, 4)
        overlapped.OffsetLow <- BitConverter.ToInt32(bytes, 0)
    else
        overlapped.OffsetHigh <- BitConverter.ToInt32(bytes, 0)
        overlapped.OffsetLow <- BitConverter.ToInt32(bytes, 4)        

    let handle = GCHandle.Alloc(overlapped, GCHandleType.Pinned)
    let mutable read = 0
    let succeeded = ReadFile(hFile, lpBuffer, nNumberOfBytesToRead, &read, handle.AddrOfPinnedObject())
    handle.Free()
    if succeeded
    then read
    else -1

let ReadHDAsSeq handle =    
    let chank = 128 * 1024 * 1024
    let buf = Array.zeroCreate chank
    let offset = ref 0L
    let flg = ref false
    seq { 
            while not !flg do
                let read = ReadFileW(handle, buf, chank, !offset)
                offset := !offset + int64 chank
                flg := read < chank || read = -1
                yield! buf
        }

let ReadHD handle =
    let offset = ref 0L    
    fun (buf:array<_>) ->
        let chank = buf.Length
        let read = ReadFileW(handle, buf, chank, !offset)
        offset := !offset + int64 chank
        if read = -1
        then None else Some buf        