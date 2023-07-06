namespace Brahma.FSharp

open Brahma.FSharp.OpenCL.Translator
open OpenCL.Net

/// Represents an abstraction over OpenCL context.
type ClContext(clDevice: ClDevice, ?translator, ?compilerOptions: string) =
    let translator = defaultArg translator <| FSQuotationToOpenCLTranslator(clDevice)

    let context =
        let error = ref Unchecked.defaultof<ErrorCode>

        let ctx =
            Cl.CreateContext(null, 1u, [| clDevice.Device |], null, System.IntPtr.Zero, error)

        if error.Value <> ErrorCode.Success then
            raise <| Cl.Exception error.Value

        ctx

    member val QueueProvider = CommandQueueProvider(clDevice.Device, context, translator)

    member this.ClDevice = clDevice

    member this.Context = context

    member this.Translator = translator

    member this.CompilerOptions = compilerOptions

    override this.ToString() =
        let mutable e = ErrorCode.Unknown
        let context = this
        let device = context.ClDevice.Device
        let deviceName = Cl.GetDeviceInfo(device, DeviceInfo.Name, &e).ToString()

        if deviceName.Length < 20 then
            $"%s{deviceName}"
        else
            let platform = Cl.GetDeviceInfo(device, DeviceInfo.Platform, &e).CastTo<Platform>()
            let platformName = Cl.GetPlatformInfo(platform, PlatformInfo.Name, &e).ToString()

            let deviceType =
                match Cl.GetDeviceInfo(device, DeviceInfo.Type, &e).CastTo<DeviceType>() with
                | DeviceType.Cpu -> "CPU"
                | DeviceType.Gpu -> "GPU"
                | DeviceType.Accelerator -> "Accelerator"
                | _ -> "another"

            $"%s{platformName}, %s{deviceType}"
