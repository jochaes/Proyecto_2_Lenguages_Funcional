﻿namespace PalabrasEnredadas

open System.Reflection
open System.Runtime.CompilerServices
open Android.App

// the name of the type here needs to match the name inside the ResourceDesigner attribute
type Resources = PalabrasEnredadas.Resource
type Resources2 = PalabrasEnredadas.Resource
type Resources3 = PalabrasEnredadas.Resource

[<assembly: Android.Runtime.ResourceDesigner("PalabrasEnredadas.Resources", IsApplication=true)>]

// Information about this assembly is defined by the following attributes. 
// Change them to the values specific to your project.

[<assembly: AssemblyTitle("PalabrasEnredadas")>]
[<assembly: AssemblyDescription("")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("")>]
[<assembly: AssemblyCopyright("${AuthorCopyright}")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]

// The assembly version has the format "{Major}.{Minor}.{Build}.{Revision}".
// The form "{Major}.{Minor}.*" will automatically update the build and revision,
// and "{Major}.{Minor}.{Build}.*" will update just the revision.

[<assembly: AssemblyVersion("1.0.0")>]

// The following attributes are used to specify the signing key for the assembly, 
// if desired. See the Mono documentation for more information about signing.

//[<assembly: AssemblyDelaySign(false)>]
//[<assembly: AssemblyKeyFile("")>]

()


