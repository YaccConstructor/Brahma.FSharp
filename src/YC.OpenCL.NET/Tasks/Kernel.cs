#region License and Copyright Notice
// Copyright (c) 2010 Ananth B.
// All rights reserved.
// 
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
// 
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
// 
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.
#endregion

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.CodeDom.Compiler;

using System.IO;
using System.Text.RegularExpressions;

using OpenCL.Net.Extensions;
using Microsoft.CSharp;
using Microsoft.Build.Framework;
using System.CodeDom;
using System.Runtime.CompilerServices;

[assembly: InternalsVisibleTo("OpenCL.NetCore.Tests")]

namespace OpenCL.Net.Tasks
{
    public sealed class Kernel : ITask
    {
        public IBuildEngine BuildEngine { get; set; }
        public ITaskHost HostObject { get; set; }

        public ITaskItem[] InputFiles { get; set; }

        [Output] public ITaskItem[] OutputFiles { get; set; }

        private const string MetadataLink = "Link";
        private const string MetadataCopyToOutputDirectory = "CopyToOutputDirectory";
        private const string MetadataFullPath = "FullPath";
        private const string MetadataIdentity = "Identity";

        public bool Execute()
        {
            var projectDir = Path.GetDirectoryName(BuildEngine.ProjectFileOfTaskNode);

            for (int i = 0; i < InputFiles.Length; i++)
            {
                BuildEngine.LogMessageEvent(new BuildMessageEventArgs(string.Format("Generating kernel wrappers for {0}", Path.GetFileName(InputFiles[i].ItemSpec)), "Kernel", "OpenCL.Net", MessageImportance.High));
                
                foreach (var mi in InputFiles[i].MetadataNames)
                    BuildEngine.LogMessageEvent(new BuildMessageEventArgs(string.Format("\tMetadata: {0}: {1}", mi.ToString(), InputFiles[i].GetMetadata(mi.ToString())), "Kernel", "OpenCL.Net", MessageImportance.High));

                var link = InputFiles[i].GetMetadata(MetadataLink);
                var isLink = !string.IsNullOrEmpty(link);
                var identity = InputFiles[i].GetMetadata(MetadataIdentity);
                var copy = InputFiles[i].GetMetadata(MetadataCopyToOutputDirectory);
                var copyToOutputDirectory = !string.IsNullOrEmpty(copy) || (copy == "PreserveNewest") || (copy == "Always");
                var fullPath = InputFiles[i].GetMetadata(MetadataFullPath);

                var embedSource = !copyToOutputDirectory;
                var outputPath = isLink ? link : identity;
                File.WriteAllText(OutputFiles[i].ItemSpec,
                    ProcessKernelFile(fullPath, File.ReadAllText(InputFiles[i].ItemSpec), embedSource, outputPath));

                if (copyToOutputDirectory)
                {
                    InputFiles[i].RemoveMetadata(MetadataCopyToOutputDirectory);
                    OutputFiles[i].RemoveMetadata(MetadataCopyToOutputDirectory);
                }
            }

            BuildEngine.LogMessageEvent(new BuildMessageEventArgs("Done.", "Kernel", "OpenCL.Net", MessageImportance.High));
            return true;
        }

        private const string KernelName = "kernelName";
        private const string Qualifier = "qualifier";
        private const string Datatype = "datatype";
        private const string Pointer = "pointer";
        private const string Identifier = "identifier";
        private const string VectorWidth = "vectorWidth";

        private static readonly Regex _kernelParser = new Regex(@"(__)?kernel\s+void\s+(?<kernelName>[\w_]+)\s*\((\s*(__)?(?<qualifier>((?<qual>(global|local|read_only|write_only))\s+)?(?(qual)|(\.?)))(?<datatype>(bool|char|unsigned char|uchar|short|unsigned short|ushort|float|double|int|unsigned int|uint|long|unsigned long|ulong|size_t|image2d_t|image3d_t|sampler_t))(?<vectorWidth>(16|2|3|4|8)?)\s*(?<pointer>\*?)\s*(?<identifier>[_\w]+)\s*,?\s*)+\)",
            RegexOptions.Compiled | RegexOptions.ExplicitCapture);
        private static readonly Regex _stripLineBreaksInKernelSignature = new Regex(@"(__)?kernel(?:\r\n)*.+(?:\r\n)*\((?:~[_a-zA-Z0-9])*([_a-zA-Z0-9*\s]+,?(?:~[_a-zA-Z0-9])*)+");
        
        private const string blockComments = @"/\*(.*?)\*/";
        private const string lineComments = @"//(.*?)\r?\n";
        private const string strings = @"""((\\[^\n]|[^""\n])*)""";

        private static void StripKernelSignatureLinebreaks(ref string original)
        {
            var match = _stripLineBreaksInKernelSignature.Match(original);
            foreach (Capture capture in match.Groups[2].Captures)
                original = original.Replace(capture.Value, capture.Value.Trim());
        }

        private static void StripComments(ref string original)
        {
            original = Regex.Replace(original,
                blockComments + "|" + lineComments + "|" + strings,
                me =>
                {
                    if (me.Value.StartsWith("/*") || me.Value.StartsWith("//"))
                        return me.Value.StartsWith("//") ? System.Environment.NewLine : "";
                    // Keep the literal strings
                    return me.Value;
                },
                RegexOptions.Singleline);
        }

        private static string GenerateCSharpCode(System.CodeDom.CodeCompileUnit compileunit)
        {
            var provider = new CSharpCodeProvider();

            var sb = new StringBuilder();
            using (var sw = new StringWriter(sb))
            using (var tw = new IndentedTextWriter(sw, "    "))
                provider.GenerateCodeFromCompileUnit(compileunit, tw,
                    new CodeGeneratorOptions());

            return sb.ToString();
        }

        private static string TranslateType(string clType, int vectorWidth)
        {
            if (vectorWidth == 0)
                switch (clType)
                {
                    case "bool": return typeof(bool).FullName;

                    case "char": return typeof(char).FullName;

                    case "unsigned char":
                    case "uchar": return typeof(byte).FullName;

                    case "short": return typeof(short).FullName;

                    case "unsigned short":
                    case "ushort": return typeof(ushort).FullName;

                    case "int": return typeof(int).FullName;

                    case "unsigned int":
                    case "uint": return typeof(uint).FullName;

                    case "long": return typeof(long).FullName;

                    case "unsigned long":
                    case "ulong": return typeof(ulong).FullName;

                    case "float": return typeof(float).FullName;

                    case "size_t": return typeof(IntPtr).FullName;

                    case "image2d_t":
                    case "image3d_t":
                    case "sampler_t":
                        return typeof(IMem).FullName;

                    default:
                        return "Unknown";
                }
            else
            {
                switch (clType)
                {
                    case "char":
                    case "uchar":
                    case "short":
                    case "ushort":
                    case "int":
                    case "uint":
                    case "long":
                    case "ulong":
                    case "float":
                    case "double":
                        return string.Format("{0}{1}", clType, vectorWidth);

                    default:
                        return "Unknown";
                }
            }
        }

        internal static string ProcessKernelFile(string filename, string kernelFileContents, bool embedSource = true, string outputPath = null)
        {
            var kernelFilename = Path.GetFileNameWithoutExtension(filename);

            var codeUnit = new CodeCompileUnit();
            var ns = new CodeNamespace(kernelFilename);
            codeUnit.Namespaces.Add(ns);
            
            // Strip comments
            StripComments(ref kernelFileContents);

            // Ensure that kernel signatures do not have line breaks
            StripKernelSignatureLinebreaks(ref kernelFileContents);

            var lines = kernelFileContents.Split(new[] { System.Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);

            foreach (var line in lines)
            {
                var match = _kernelParser.Match(line);
                if (match.Success)
                {
                    var kernelName = match.Groups[KernelName].Value;
                    var kernel = new CodeTypeDeclaration(kernelName);
                    ns.Types.Add(kernel);
                    ns.Imports.Add(new CodeNamespaceImport("System.IO"));
                    ns.Imports.Add(new CodeNamespaceImport("OpenCL.Net"));
                    ns.Imports.Add(new CodeNamespaceImport("OpenCL.Net.Extensions"));

                    kernel.Attributes = MemberAttributes.Public | MemberAttributes.Final;
                    kernel.BaseTypes.Add(typeof(KernelWrapperBase));

                    var kernelPathProperty = new CodeMemberProperty
                    {
                        Attributes = MemberAttributes.Override | MemberAttributes.FamilyOrAssembly,
                        Name = "KernelPath",
                        Type = new CodeTypeReference(typeof(string), CodeTypeReferenceOptions.GlobalReference),
                        HasGet = true,
                        HasSet = false
                    };
                    kernelPathProperty.GetStatements.Add(new CodeMethodReturnStatement(new CodeMethodInvokeExpression(new CodeTypeReferenceExpression(typeof(Path)), "Combine",
                        new CodeSnippetExpression("System.AppDomain.CurrentDomain.BaseDirectory"),
                        new CodePrimitiveExpression(outputPath))));
                    kernel.Members.Add(kernelPathProperty);

                    var originalKernelPath = new CodeMemberProperty
                    {
                        Attributes = MemberAttributes.Override | MemberAttributes.FamilyOrAssembly,
                        Name = "OriginalKernelPath",
                        Type = new CodeTypeReference(typeof(string), CodeTypeReferenceOptions.GlobalReference),
                        HasGet = true,
                        HasSet = false
                    };
                    originalKernelPath.GetStatements.Add(new CodeMethodReturnStatement(new CodePrimitiveExpression(filename)));
                    kernel.Members.Add(originalKernelPath);

                    var kernelSource = new CodeMemberProperty 
                    { 
                        Attributes = MemberAttributes.Override | MemberAttributes.FamilyOrAssembly,
                        Name = "KernelSource",
                        Type = new CodeTypeReference(typeof(string), CodeTypeReferenceOptions.GlobalReference),
                        HasGet = true,
                        HasSet = false
                    };
                    kernelSource.GetStatements.Add(new CodeMethodReturnStatement(
                        embedSource ?
                            (CodeExpression)new CodePrimitiveExpression(kernelFileContents) :
                            new CodeMethodInvokeExpression(new CodeTypeReferenceExpression(typeof(Path)), "Combine",
                        new CodeSnippetExpression("System.AppDomain.CurrentDomain.BaseDirectory"),
                        new CodePrimitiveExpression(outputPath))));
                    kernel.Members.Add(kernelSource);

                    var kernelNameProperty = new CodeMemberProperty
                    {
                        Attributes = MemberAttributes.Override | MemberAttributes.FamilyOrAssembly,
                        Name = "KernelName",
                        Type = new CodeTypeReference(typeof(string), CodeTypeReferenceOptions.GlobalReference),
                        HasGet = true,
                        HasSet = false
                    };
                    kernelNameProperty.GetStatements.Add(new CodeMethodReturnStatement(new CodePrimitiveExpression(kernelName)));
                    kernel.Members.Add(kernelNameProperty);

                    var constructor = new CodeConstructor();
                    kernel.Members.Add(constructor);

                    var constructorParams = new CodeParameterDeclarationExpression(typeof(Context), "context");
                    constructor.Parameters.Add(constructorParams);
                    constructor.BaseConstructorArgs.Add(new CodeVariableReferenceExpression("context"));
                    constructor.Attributes = MemberAttributes.Public;

                    var executePrivateMethod = new CodeMemberMethod
                    {
                        Name = "run",
                        Attributes = MemberAttributes.Private | MemberAttributes.Final,
                        ReturnType = new CodeTypeReference(typeof(Event))
                    };
                    var run1D = new CodeMemberMethod
                    {
                        Name = "Run",
                        Attributes = MemberAttributes.Public | MemberAttributes.Final,
                    };
                    var enqueueRun1D = new CodeMemberMethod
                    {
                        Name = "EnqueueRun",
                        Attributes = MemberAttributes.Public | MemberAttributes.Final,
                        ReturnType = new CodeTypeReference(typeof(Event))
                    };

                    var run2D = new CodeMemberMethod
                    {
                        Name = "Run",
                        Attributes = MemberAttributes.Public | MemberAttributes.Final,
                    };
                    var enqueueRun2D = new CodeMemberMethod
                    {
                        Name = "EnqueueRun",
                        Attributes = MemberAttributes.Public | MemberAttributes.Final,
                        ReturnType = new CodeTypeReference(typeof(Event))
                    };

                    var run3D = new CodeMemberMethod
                    {
                        Name = "Run",
                        Attributes = MemberAttributes.Public | MemberAttributes.Final,
                    };
                    var enqueueRun3D = new CodeMemberMethod
                    {
                        Name = "EnqueueRun",
                        Attributes = MemberAttributes.Public | MemberAttributes.Final,
                        ReturnType = new CodeTypeReference(typeof(Event))
                    };

                    var commandQueueParameter = new CodeParameterDeclarationExpression(typeof(CommandQueue), "commandQueue");
                    executePrivateMethod.Parameters.Add(commandQueueParameter);

                    run1D.Parameters.Add(commandQueueParameter);
                    enqueueRun1D.Parameters.Add(commandQueueParameter);

                    run2D.Parameters.Add(commandQueueParameter);
                    enqueueRun2D.Parameters.Add(commandQueueParameter);

                    run3D.Parameters.Add(commandQueueParameter);
                    enqueueRun3D.Parameters.Add(commandQueueParameter);

                    var callPrivateExecute1D = new CodeMethodInvokeExpression(new CodeMethodReferenceExpression(new CodeThisReferenceExpression(), "run"));
                    var callPrivateExecute2D = new CodeMethodInvokeExpression(new CodeMethodReferenceExpression(new CodeThisReferenceExpression(), "run"));
                    var callPrivateExecute3D = new CodeMethodInvokeExpression(new CodeMethodReferenceExpression(new CodeThisReferenceExpression(), "run"));
                    callPrivateExecute1D.Parameters.Add(new CodeArgumentReferenceExpression("commandQueue"));
                    callPrivateExecute2D.Parameters.Add(new CodeArgumentReferenceExpression("commandQueue"));
                    callPrivateExecute3D.Parameters.Add(new CodeArgumentReferenceExpression("commandQueue"));

                    run1D.Statements.Add(new CodeVariableDeclarationStatement(new CodeTypeReference(typeof(Event)), "ev", callPrivateExecute1D));
                    run1D.Statements.Add(new CodeSnippetExpression("ev.Wait()"));
                    enqueueRun1D.Statements.Add(new CodeMethodReturnStatement(callPrivateExecute1D));

                    run2D.Statements.Add(new CodeVariableDeclarationStatement(new CodeTypeReference(typeof(Event)), "ev", callPrivateExecute2D));
                    run2D.Statements.Add(new CodeSnippetExpression("ev.Wait()"));
                    enqueueRun2D.Statements.Add(new CodeMethodReturnStatement(callPrivateExecute2D));

                    run3D.Statements.Add(new CodeVariableDeclarationStatement(new CodeTypeReference(typeof(Event)), "ev", callPrivateExecute3D));
                    run3D.Statements.Add(new CodeSnippetExpression("ev.Wait()"));
                    enqueueRun3D.Statements.Add(new CodeMethodReturnStatement(callPrivateExecute3D));

                    kernel.Members.Add(executePrivateMethod);
                    kernel.Members.Add(run1D);
                    kernel.Members.Add(enqueueRun1D);
                    kernel.Members.Add(run2D);
                    kernel.Members.Add(enqueueRun2D);
                    kernel.Members.Add(run3D);
                    kernel.Members.Add(enqueueRun3D);

                    for (int i = 0; i < match.Groups[Identifier].Captures.Count; i++)
                    {
                        bool isPointer = !string.IsNullOrEmpty(match.Groups[Pointer].Captures[i].Value);
                        var rawDatatype = match.Groups[Datatype].Captures[i].Value;
                        var name = match.Groups[Identifier].Captures[i].Value;
                        var vectorWidth = match.Groups[VectorWidth].Captures[i].Value == string.Empty ? 0 : int.Parse(match.Groups[VectorWidth].Captures[i].Value);
                        var qualifier = match.Groups[Qualifier].Captures[i].Value.Trim();
                        var local = false;

                        CodeParameterDeclarationExpression parameter = null;
                        switch (qualifier)
                        {
                            case "global":
                                parameter = new CodeParameterDeclarationExpression(string.Format("OpenCL.Net.IMem<{0}>", TranslateType(rawDatatype, vectorWidth)), name);
                                break;
                            case "local":
                                local = true;
                                name = name + "_length";
                                parameter = new CodeParameterDeclarationExpression(typeof(int), name);
                                break;
                            case "read_only":
                            case "write_only":
                            case "":
                                parameter = new CodeParameterDeclarationExpression(TranslateType(rawDatatype, vectorWidth), name);
                                break;
                        }
                        if (parameter != null)
                        {
                            executePrivateMethod.Parameters.Add(parameter);
                            run1D.Parameters.Add(parameter);
                            enqueueRun1D.Parameters.Add(parameter);
                            run2D.Parameters.Add(parameter);
                            enqueueRun2D.Parameters.Add(parameter);
                            run3D.Parameters.Add(parameter);
                            enqueueRun3D.Parameters.Add(parameter);

                            callPrivateExecute1D.Parameters.Add(new CodeArgumentReferenceExpression(name));
                            callPrivateExecute2D.Parameters.Add(new CodeArgumentReferenceExpression(name));
                            callPrivateExecute3D.Parameters.Add(new CodeArgumentReferenceExpression(name));
                        }

                        var setArgument = local ?
                            new CodeMethodInvokeExpression(new CodeSnippetExpression("OpenCL.Net.Cl"), "SetKernelArg",
                                new CodePropertyReferenceExpression(new CodeThisReferenceExpression(), "Kernel"), new CodeSnippetExpression(i.ToString()),
                                new CodeCastExpression(new CodeTypeReference(typeof(IntPtr)), new CodeBinaryOperatorExpression(new CodeArgumentReferenceExpression(name), 
                                    CodeBinaryOperatorType.Multiply,
                                    new CodeSnippetExpression(string.Format("OpenCL.Net.TypeSize<{0}>.SizeInt", TranslateType(rawDatatype, vectorWidth))))),
                                new CodeSnippetExpression("null")) :
                            new CodeMethodInvokeExpression(new CodeSnippetExpression("OpenCL.Net.Cl"), "SetKernelArg",
                                new CodePropertyReferenceExpression(new CodeThisReferenceExpression(), "Kernel"), new CodeSnippetExpression(i.ToString()),
                                new CodeArgumentReferenceExpression(name));
                        executePrivateMethod.Statements.Add(setArgument);
                    }

                    executePrivateMethod.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize0"));
                    executePrivateMethod.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize1 = 0"));
                    executePrivateMethod.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize2 = 0"));

                    executePrivateMethod.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize0 = 0"));
                    executePrivateMethod.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize1 = 0"));
                    executePrivateMethod.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize2 = 0"));

                    var eventWaitListParam = new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(Event[])), "waitFor");
                    eventWaitListParam.CustomAttributes.Add(new CodeAttributeDeclaration("System.ParamArrayAttribute"));
                    executePrivateMethod.Parameters.Add(eventWaitListParam);
                    executePrivateMethod.Statements.Add(new CodeVariableDeclarationStatement(typeof(Event), "ev"));
                    executePrivateMethod.Statements.Add(new CodeVariableDeclarationStatement(typeof(ErrorCode), "err"));
                    executePrivateMethod.Statements.Add(new CodeAssignStatement(
                        new CodeVariableReferenceExpression("err"),
                        // = 
                        new CodeMethodInvokeExpression(new CodeSnippetExpression("OpenCL.Net.Cl"), "EnqueueNDRangeKernel",
                            new CodeArgumentReferenceExpression("commandQueue"),
                            new CodePropertyReferenceExpression(new CodeThisReferenceExpression(), "Kernel"),
                            new CodeMethodInvokeExpression(new CodeBaseReferenceExpression(), "GetWorkDimension",
                                new CodeArgumentReferenceExpression("globalWorkSize0"),
                                new CodeArgumentReferenceExpression("globalWorkSize1"),
                                new CodeArgumentReferenceExpression("globalWorkSize2")),
                            new CodeSnippetExpression("null"),
                            new CodeMethodInvokeExpression(new CodeBaseReferenceExpression(), "GetWorkSizes",
                                new CodeArgumentReferenceExpression("globalWorkSize0"),
                                new CodeArgumentReferenceExpression("globalWorkSize1"),
                                new CodeArgumentReferenceExpression("globalWorkSize2")),
                            new CodeMethodInvokeExpression(new CodeBaseReferenceExpression(), "GetWorkSizes",
                                new CodeArgumentReferenceExpression("localWorkSize0"),
                                new CodeArgumentReferenceExpression("localWorkSize1"),
                                new CodeArgumentReferenceExpression("localWorkSize2")),

                            new CodeCastExpression(new CodeTypeReference(typeof(uint)),
                                new CodeSnippetExpression("waitFor.Length")),
                            new CodeArgumentReferenceExpression("waitFor.Length == 0 ? null : waitFor"),
                            new CodeVariableReferenceExpression("out ev"))));
                    executePrivateMethod.Statements.Add(new CodeMethodInvokeExpression(new CodeSnippetExpression("OpenCL.Net.Cl"), "Check", new CodeVariableReferenceExpression("err")));
                    executePrivateMethod.Statements.Add(new CodeMethodReturnStatement(new CodeVariableReferenceExpression("ev")));

                    run1D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize"));
                    enqueueRun1D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize"));
                    run1D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize = 0"));
                    enqueueRun1D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize = 0"));
                    run1D.Parameters.Add(eventWaitListParam);
                    enqueueRun1D.Parameters.Add(eventWaitListParam);
                    callPrivateExecute1D.Parameters.Add(new CodeArgumentReferenceExpression("globalWorkSize0: globalWorkSize"));
                    callPrivateExecute1D.Parameters.Add(new CodeArgumentReferenceExpression("localWorkSize0: localWorkSize"));
                    callPrivateExecute1D.Parameters.Add(new CodeArgumentReferenceExpression("waitFor: waitFor"));

                    run2D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize0"));
                    enqueueRun2D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize0"));
                    run2D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize1"));
                    enqueueRun2D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize1"));
                    run2D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize0 = 0"));
                    enqueueRun2D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize0 = 0"));
                    run2D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize1 = 0"));
                    enqueueRun2D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize1 = 0"));
                    run2D.Parameters.Add(eventWaitListParam);
                    enqueueRun2D.Parameters.Add(eventWaitListParam);

                    callPrivateExecute2D.Parameters.Add(new CodeArgumentReferenceExpression("globalWorkSize0: globalWorkSize0"));
                    callPrivateExecute2D.Parameters.Add(new CodeArgumentReferenceExpression("globalWorkSize1: globalWorkSize1"));
                    callPrivateExecute2D.Parameters.Add(new CodeArgumentReferenceExpression("localWorkSize0: localWorkSize0"));
                    callPrivateExecute2D.Parameters.Add(new CodeArgumentReferenceExpression("localWorkSize1: localWorkSize1"));
                    callPrivateExecute2D.Parameters.Add(new CodeArgumentReferenceExpression("waitFor: waitFor"));

                    run3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize0"));
                    enqueueRun3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize0"));
                    run3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize1"));
                    enqueueRun3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize1"));
                    run3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize2"));
                    enqueueRun3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "globalWorkSize2"));
                    run3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize0 = 0"));
                    enqueueRun3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize0 = 0"));
                    run3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize1 = 0"));
                    enqueueRun3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize1 = 0"));
                    run3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize2 = 0"));
                    enqueueRun3D.Parameters.Add(new CodeParameterDeclarationExpression(new CodeTypeReference(typeof(uint)), "localWorkSize2 = 0"));
                    run3D.Parameters.Add(eventWaitListParam);
                    enqueueRun3D.Parameters.Add(eventWaitListParam);
                    callPrivateExecute3D.Parameters.Add(new CodeArgumentReferenceExpression("globalWorkSize0: globalWorkSize0"));
                    callPrivateExecute3D.Parameters.Add(new CodeArgumentReferenceExpression("globalWorkSize1: globalWorkSize1"));
                    callPrivateExecute3D.Parameters.Add(new CodeArgumentReferenceExpression("globalWorkSize2: globalWorkSize2"));
                    callPrivateExecute3D.Parameters.Add(new CodeArgumentReferenceExpression("localWorkSize0: localWorkSize0"));
                    callPrivateExecute3D.Parameters.Add(new CodeArgumentReferenceExpression("localWorkSize1: localWorkSize1"));
                    callPrivateExecute3D.Parameters.Add(new CodeArgumentReferenceExpression("localWorkSize2: localWorkSize2"));
                    callPrivateExecute3D.Parameters.Add(new CodeArgumentReferenceExpression("waitFor: waitFor"));
                }
            }

            return GenerateCSharpCode(codeUnit);
        }
    }
}
