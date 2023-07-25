# Installation

    [lang=bash]
    dotnet add package Brahma.FSharp

Or

    [lang=bash]
    paket install Brahma.FSharp

More options [here](https://www.nuget.org/packages/Brahma.FSharp/).

# Troubleshooting

## Timeout

Sometimes calculations could be interrupted buy GPU driver (OS) timeout (TDR). 
For hot fix you can set TdrLevel registry key (KeyPath : HKEY\_LOCAL\_MACHINE\System\CurrentControlSet\Control\GraphicsDrivers) value to 0. 
If this key is not exists, then you should create it. For more details look at ["TDR Registry Keys (Windows Drivers)"](https://msdn.microsoft.com/en-us/library/windows/hardware/ff569918(v=vs.85).aspx).

## OpenCL.dll

If you want to use Brahma.FSharp on Linux/macOS, set ```BRAHMA_OCL_PATH``` environment variable to ```opencl.dll``` location.

