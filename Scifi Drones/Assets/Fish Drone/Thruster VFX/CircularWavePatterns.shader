// Made with Amplify Shader Editor v1.9.6.3
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "CircularWavePatterns"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_Scale("Scale", Range( 0 , 10)) = 1
		_RingTexture("RingTexture", 2D) = "white" {}
		_RotateTiledTexture("RotateTiledTexture", Range( 0 , 1)) = 0
		_TextureScale("TextureScale", Range( 0 , 4)) = 1
		_TexCircularSpeed("TexCircularSpeed", Range( -24 , 24)) = 0
		_UsePolarCoordinates("UsePolarCoordinates", Int) = 0
		_TexRadialSpeed("TexRadialSpeed", Range( -10 , 10)) = 1
		_PolarLength("Polar Length", Int) = 1
		_Waves("Waves", Int) = 0
		_WaveStrengthCenter("WaveStrengthCenter", Range( 0 , 4)) = 0
		_WaveStrengthEdge("WaveStrengthEdge", Range( 0 , 4)) = 0
		_WaveStrengthChangePower("WaveStrengthChangePower", Range( 0.01 , 5)) = 1
		_WavesRotationsPerSec("WavesRotationsPerSec", Range( 0 , 2)) = 0
		_WaveOffsetPerObject("WaveOffsetPerObject", Range( 0 , 10)) = 0
		_ValueMultiStartPosition("ValueMultiStartPosition", Range( 0 , 1)) = 0
		_ValueMultiStartFactor("ValueMultiStartFactor", Range( 0 , 5)) = 1
		_ValueMultiEndPosition("ValueMultiEndPosition", Range( 0 , 1)) = 1
		_ValueMultiEndFactor("ValueMultiEndFactor", Range( 0 , 5)) = 0
		_ValueMulitPower("ValueMulitPower", Range( 0.01 , 30)) = 1
		_AlphaMultiStartPosition("AlphaMultiStartPosition", Range( 0 , 1)) = 0
		_AlphaMultiStartFactor("AlphaMultiStartFactor", Range( 0 , 1)) = 1
		_AlphaMultiEndPosition("AlphaMultiEndPosition", Range( 0 , 1)) = 1
		_AlphaMultiEndFactor("AlphaMultiEndFactor", Range( 0 , 1)) = 1
		_AlphaMultiPower("AlphaMultiPower", Range( 0.01 , 30)) = 1
		[HDR]_Color1("Color1", Color) = (1,1,1,0)
		_C1StartLerp("C1 Start Lerp", Range( 0 , 1)) = 0
		_C1EndLerp("C1 End Lerp", Range( 0 , 1)) = 0
		[HDR]_Color2("Color2", Color) = (0.2301886,0.2301886,0.2301886,0)
		_C2StartLerp("C2 Start Lerp", Range( 0 , 1)) = 0
		_C2EndLerp("C2 End Lerp", Range( 0 , 1)) = 0
		[HDR]_Color3("Color3", Color) = (0,0,0,0)
		_MoveCenterDown("MoveCenterDown", Range( 0 , 100)) = 0
		_MoveCenterDownPower("MoveCenterDownPower", Range( 0.1 , 20)) = 1


		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25

		[HideInInspector] _QueueOffset("_QueueOffset", Float) = 0
        [HideInInspector] _QueueControl("_QueueControl", Float) = -1

        [HideInInspector][NoScaleOffset] unity_Lightmaps("unity_Lightmaps", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_LightmapsInd("unity_LightmapsInd", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_ShadowMasks("unity_ShadowMasks", 2DArray) = "" {}

		//[HideInInspector][ToggleUI] _AddPrecomputedVelocity("Add Precomputed Velocity", Float) = 1
		[HideInInspector][ToggleOff] _ReceiveShadows("Receive Shadows", Float) = 1.0
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" "UniversalMaterialType"="Unlit" }

		Cull Off
		AlphaToMask Off

		

		HLSLINCLUDE
		#pragma target 4.5
		#pragma prefer_hlslcc gles
		// ensure rendering platforms toggle list is visible

		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Common.hlsl"
		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Filtering.hlsl"

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}

		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForwardOnly" }

			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			

			HLSLPROGRAM

			#pragma shader_feature_local _RECEIVE_SHADOWS_OFF
			#pragma multi_compile_instancing
			#pragma instancing_options renderinglayer
			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 170003


			#pragma multi_compile_fragment _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile_fragment _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3
			#pragma multi_compile_fragment _ _GBUFFER_NORMALS_OCT

			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
            #pragma multi_compile _ LIGHTMAP_ON
            #pragma multi_compile _ DYNAMICLIGHTMAP_ON
			#pragma multi_compile_fragment _ DEBUG_DISPLAY

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_UNLIT

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Input.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DBuffer.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Debug/Debugging3D.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/SurfaceData.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				float4 clipPosV : TEXCOORD0;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 positionWS : TEXCOORD1;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD2;
				#endif
				#ifdef ASE_FOG
					float fogFactor : TEXCOORD3;
				#endif
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color2;
			float4 _Color1;
			float _Scale;
			float _AlphaMultiStartPosition;
			float _AlphaMultiEndFactor;
			float _AlphaMultiStartFactor;
			float _C2EndLerp;
			float _C2StartLerp;
			float _C1StartLerp;
			float _C1EndLerp;
			float _RotateTiledTexture;
			int _PolarLength;
			float _TextureScale;
			float _TexCircularSpeed;
			float _TexRadialSpeed;
			int _UsePolarCoordinates;
			float _ValueMulitPower;
			float _ValueMultiEndPosition;
			float _ValueMultiStartPosition;
			float _ValueMultiEndFactor;
			float _ValueMultiStartFactor;
			float _MoveCenterDownPower;
			float _MoveCenterDown;
			float _WavesRotationsPerSec;
			int _Waves;
			float _WaveOffsetPerObject;
			float _WaveStrengthChangePower;
			float _WaveStrengthEdge;
			float _WaveStrengthCenter;
			float _AlphaMultiEndPosition;
			float _AlphaMultiPower;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _RingTexture;


			
			PackedVaryings VertexFunction( Attributes input  )
			{
				PackedVaryings output = (PackedVaryings)0;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float3 temp_output_193_0 = ( input.positionOS.xyz * _Scale );
				float3 Scaled_Vertices_Local303 = temp_output_193_0;
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult569 = lerp( _WaveStrengthCenter , _WaveStrengthEdge , pow( Distance_To_Center_01396 , _WaveStrengthChangePower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_489_0 = ( ase_objectScale.y * 10000.0 );
				float RandomY2492 = ( temp_output_489_0 - floor( temp_output_489_0 ) );
				float2 appendResult45 = (float2(input.positionOS.xyz.x , input.positionOS.xyz.z));
				float2 normalizeResult17 = normalize( appendResult45 );
				float2 break18 = normalizeResult17;
				float2 _Vector0 = float2(0,1);
				float dotResult19 = dot( normalizeResult17 , _Vector0 );
				float temp_output_16_0 = atan2( ( ( break18.x * _Vector0.y ) - ( break18.y * _Vector0.x ) ) , dotResult19 );
				float Angle_Atan161 = temp_output_16_0;
				float mulTime85 = _TimeParameters.x * ( _WavesRotationsPerSec * ( 2.0 * PI ) * _Waves );
				float temp_output_57_0 = sin( ( ( RandomY2492 * _WaveOffsetPerObject ) + ( ( Angle_Atan161 * _Waves ) + mulTime85 ) ) );
				float Vertical_Offset_For_Waves152 = ( lerpResult569 * temp_output_57_0 );
				float temp_output_591_0 = ( 1.0 - Distance_To_Center_01396 );
				float3 appendResult60 = (float3(0.0 , ( Vertical_Offset_For_Waves152 - ( temp_output_591_0 * pow( _MoveCenterDown , _MoveCenterDownPower ) ) ) , 0.0));
				
				output.ase_texcoord4 = input.positionOS;
				output.ase_texcoord5.xy = input.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord5.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ( Scaled_Vertices_Local303 + appendResult60 );

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				VertexPositionInputs vertexInput = GetVertexPositionInputs( input.positionOS.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					output.positionWS = vertexInput.positionWS;
				#endif

				#ifdef ASE_FOG
					output.fogFactor = ComputeFogFactor( vertexInput.positionCS.z );
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					output.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				output.positionCS = vertexInput.positionCS;
				output.clipPosV = vertexInput.positionCS;
				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.vertex = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			half4 frag ( PackedVaryings input
				#ifdef _WRITE_RENDERING_LAYERS
				, out float4 outRenderingLayers : SV_Target1
				#endif
				 ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( input );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( input );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 WorldPosition = input.positionWS;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				float4 ClipPos = input.clipPosV;
				float4 ScreenPos = ComputeScreenPos( input.clipPosV );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = input.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float3 temp_output_193_0 = ( input.ase_texcoord4.xyz * _Scale );
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult565 = lerp( _ValueMultiStartFactor , _ValueMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _ValueMultiStartPosition) * (1.0 - 0.0) / (_ValueMultiEndPosition - _ValueMultiStartPosition)) ) , _ValueMulitPower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_485_0 = ( ase_objectScale.y * 1000.0 );
				float RandomY1488 = ( temp_output_485_0 - floor( temp_output_485_0 ) );
				float mulTime512 = _TimeParameters.x * _TexRadialSpeed;
				float mulTime507 = _TimeParameters.x * _TexCircularSpeed;
				float temp_output_339_0 = ( ase_objectScale.x * 1000.0 );
				float RandomX1341 = ( temp_output_339_0 - floor( temp_output_339_0 ) );
				float CircularSpeedTimedAndOffset509 = ( mulTime507 + ( RandomX1341 * 100.0 ) );
				float2 appendResult506 = (float2(( ( RandomY1488 * 100.0 ) + mulTime512 ) , CircularSpeedTimedAndOffset509));
				float TextureScale_PT516 = _TextureScale;
				float2 temp_output_34_0_g9 = ( input.ase_texcoord5.xy - float2( 0.5,0.5 ) );
				float2 break39_g9 = temp_output_34_0_g9;
				float2 appendResult50_g9 = (float2(( TextureScale_PT516 * ( length( temp_output_34_0_g9 ) * 2.0 ) ) , ( ( atan2( break39_g9.x , break39_g9.y ) * ( 1.0 / TWO_PI ) ) * (float)_PolarLength )));
				float cos585 = cos( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float sin585 = sin( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float2 rotator585 = mul( ( appendResult506 + appendResult50_g9 ) - float2( 0.5,0.5 ) , float2x2( cos585 , -sin585 , sin585 , cos585 )) + float2( 0.5,0.5 );
				float2 texCoord443 = input.ase_texcoord5.xy * float2( 1,1 ) + float2( 0,0 );
				float cos495 = cos( CircularSpeedTimedAndOffset509 );
				float sin495 = sin( CircularSpeedTimedAndOffset509 );
				float2 rotator495 = mul( ( ( ( texCoord443 - float2( 0.5,0.5 ) ) * ( 1.0 / TextureScale_PT516 ) ) + float2( 0.5,0.5 ) ) - float2( 0.5,0.5 ) , float2x2( cos495 , -sin495 , sin495 , cos495 )) + float2( 0.5,0.5 );
				float2 temp_cast_2 = (ddx( 0.0 )).xx;
				float2 temp_cast_3 = (ddy( 0.0 )).xx;
				float4 tex2DNode66 = tex2D( _RingTexture, ( (float)_UsePolarCoordinates >= 1.0 ? rotator585 : rotator495 ), temp_cast_2, temp_cast_3 );
				float Texture_Value399 = ( ( tex2DNode66.r + tex2DNode66.g + tex2DNode66.b ) / 3.0 );
				float temp_output_17_0_g12 = ( lerpResult565 * Texture_Value399 );
				float temp_output_19_0_g12 = _C1EndLerp;
				float4 temp_output_23_0_g12 = _Color2;
				float4 lerpResult11_g12 = lerp( _Color1 , temp_output_23_0_g12 , saturate( (0.0 + (temp_output_17_0_g12 - _C1StartLerp) * (1.0 - 0.0) / (temp_output_19_0_g12 - _C1StartLerp)) ));
				float4 lerpResult6_g12 = lerp( temp_output_23_0_g12 , _Color3 , saturate( (0.0 + (temp_output_17_0_g12 - _C2StartLerp) * (1.0 - 0.0) / (_C2EndLerp - _C2StartLerp)) ));
				float4 ColorRemapped408 = ( temp_output_17_0_g12 <= temp_output_19_0_g12 ? lerpResult11_g12 : lerpResult6_g12 );
				
				float lerpResult454 = lerp( _AlphaMultiStartFactor , _AlphaMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _AlphaMultiStartPosition) * (1.0 - 0.0) / (_AlphaMultiEndPosition - _AlphaMultiStartPosition)) ) , _AlphaMultiPower ));
				float AlphaMask204 = lerpResult454;
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = ColorRemapped408.rgb;
				float Alpha = ( ColorRemapped408.a * AlphaMask204 );
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					clip( Alpha - AlphaClipThreshold );
				#endif

				#if defined(_DBUFFER)
					ApplyDecalToBaseColor(input.positionCS, Color);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( input.positionCS );
				#endif

				#ifdef ASE_FOG
					Color = MixFog( Color, input.fogFactor );
				#endif

				#ifdef _WRITE_RENDERING_LAYERS
					uint renderingLayers = GetMeshRenderingLayer();
					outRenderingLayers = float4( EncodeMeshRenderingLayer( renderingLayers ), 0, 0, 0 );
				#endif

				return half4( Color, Alpha );
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual
			AlphaToMask Off
			ColorMask 0

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 170003


			#pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_SHADOWCASTER

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 positionWS : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color2;
			float4 _Color1;
			float _Scale;
			float _AlphaMultiStartPosition;
			float _AlphaMultiEndFactor;
			float _AlphaMultiStartFactor;
			float _C2EndLerp;
			float _C2StartLerp;
			float _C1StartLerp;
			float _C1EndLerp;
			float _RotateTiledTexture;
			int _PolarLength;
			float _TextureScale;
			float _TexCircularSpeed;
			float _TexRadialSpeed;
			int _UsePolarCoordinates;
			float _ValueMulitPower;
			float _ValueMultiEndPosition;
			float _ValueMultiStartPosition;
			float _ValueMultiEndFactor;
			float _ValueMultiStartFactor;
			float _MoveCenterDownPower;
			float _MoveCenterDown;
			float _WavesRotationsPerSec;
			int _Waves;
			float _WaveOffsetPerObject;
			float _WaveStrengthChangePower;
			float _WaveStrengthEdge;
			float _WaveStrengthCenter;
			float _AlphaMultiEndPosition;
			float _AlphaMultiPower;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _RingTexture;


			
			float3 _LightDirection;
			float3 _LightPosition;

			PackedVaryings VertexFunction( Attributes input )
			{
				PackedVaryings output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( output );

				float3 temp_output_193_0 = ( input.positionOS.xyz * _Scale );
				float3 Scaled_Vertices_Local303 = temp_output_193_0;
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult569 = lerp( _WaveStrengthCenter , _WaveStrengthEdge , pow( Distance_To_Center_01396 , _WaveStrengthChangePower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_489_0 = ( ase_objectScale.y * 10000.0 );
				float RandomY2492 = ( temp_output_489_0 - floor( temp_output_489_0 ) );
				float2 appendResult45 = (float2(input.positionOS.xyz.x , input.positionOS.xyz.z));
				float2 normalizeResult17 = normalize( appendResult45 );
				float2 break18 = normalizeResult17;
				float2 _Vector0 = float2(0,1);
				float dotResult19 = dot( normalizeResult17 , _Vector0 );
				float temp_output_16_0 = atan2( ( ( break18.x * _Vector0.y ) - ( break18.y * _Vector0.x ) ) , dotResult19 );
				float Angle_Atan161 = temp_output_16_0;
				float mulTime85 = _TimeParameters.x * ( _WavesRotationsPerSec * ( 2.0 * PI ) * _Waves );
				float temp_output_57_0 = sin( ( ( RandomY2492 * _WaveOffsetPerObject ) + ( ( Angle_Atan161 * _Waves ) + mulTime85 ) ) );
				float Vertical_Offset_For_Waves152 = ( lerpResult569 * temp_output_57_0 );
				float temp_output_591_0 = ( 1.0 - Distance_To_Center_01396 );
				float3 appendResult60 = (float3(0.0 , ( Vertical_Offset_For_Waves152 - ( temp_output_591_0 * pow( _MoveCenterDown , _MoveCenterDownPower ) ) ) , 0.0));
				
				output.ase_texcoord2 = input.positionOS;
				output.ase_texcoord3.xy = input.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord3.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ( Scaled_Vertices_Local303 + appendResult60 );

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				float3 positionWS = TransformObjectToWorld( input.positionOS.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					output.positionWS = positionWS;
				#endif

				float3 normalWS = TransformObjectToWorldDir( input.normalOS );

				#if _CASTING_PUNCTUAL_LIGHT_SHADOW
					float3 lightDirectionWS = normalize(_LightPosition - positionWS);
				#else
					float3 lightDirectionWS = _LightDirection;
				#endif

				float4 positionCS = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));

				#if UNITY_REVERSED_Z
					positionCS.z = min(positionCS.z, positionCS.w * UNITY_NEAR_CLIP_VALUE);
				#else
					positionCS.z = max(positionCS.z, positionCS.w * UNITY_NEAR_CLIP_VALUE);
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					output.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				output.positionCS = positionCS;

				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.vertex = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			half4 frag(PackedVaryings input  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( input );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( input );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 WorldPosition = input.positionWS;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = input.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float3 temp_output_193_0 = ( input.ase_texcoord2.xyz * _Scale );
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult565 = lerp( _ValueMultiStartFactor , _ValueMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _ValueMultiStartPosition) * (1.0 - 0.0) / (_ValueMultiEndPosition - _ValueMultiStartPosition)) ) , _ValueMulitPower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_485_0 = ( ase_objectScale.y * 1000.0 );
				float RandomY1488 = ( temp_output_485_0 - floor( temp_output_485_0 ) );
				float mulTime512 = _TimeParameters.x * _TexRadialSpeed;
				float mulTime507 = _TimeParameters.x * _TexCircularSpeed;
				float temp_output_339_0 = ( ase_objectScale.x * 1000.0 );
				float RandomX1341 = ( temp_output_339_0 - floor( temp_output_339_0 ) );
				float CircularSpeedTimedAndOffset509 = ( mulTime507 + ( RandomX1341 * 100.0 ) );
				float2 appendResult506 = (float2(( ( RandomY1488 * 100.0 ) + mulTime512 ) , CircularSpeedTimedAndOffset509));
				float TextureScale_PT516 = _TextureScale;
				float2 temp_output_34_0_g9 = ( input.ase_texcoord3.xy - float2( 0.5,0.5 ) );
				float2 break39_g9 = temp_output_34_0_g9;
				float2 appendResult50_g9 = (float2(( TextureScale_PT516 * ( length( temp_output_34_0_g9 ) * 2.0 ) ) , ( ( atan2( break39_g9.x , break39_g9.y ) * ( 1.0 / TWO_PI ) ) * (float)_PolarLength )));
				float cos585 = cos( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float sin585 = sin( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float2 rotator585 = mul( ( appendResult506 + appendResult50_g9 ) - float2( 0.5,0.5 ) , float2x2( cos585 , -sin585 , sin585 , cos585 )) + float2( 0.5,0.5 );
				float2 texCoord443 = input.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float cos495 = cos( CircularSpeedTimedAndOffset509 );
				float sin495 = sin( CircularSpeedTimedAndOffset509 );
				float2 rotator495 = mul( ( ( ( texCoord443 - float2( 0.5,0.5 ) ) * ( 1.0 / TextureScale_PT516 ) ) + float2( 0.5,0.5 ) ) - float2( 0.5,0.5 ) , float2x2( cos495 , -sin495 , sin495 , cos495 )) + float2( 0.5,0.5 );
				float2 temp_cast_2 = (ddx( 0.0 )).xx;
				float2 temp_cast_3 = (ddy( 0.0 )).xx;
				float4 tex2DNode66 = tex2D( _RingTexture, ( (float)_UsePolarCoordinates >= 1.0 ? rotator585 : rotator495 ), temp_cast_2, temp_cast_3 );
				float Texture_Value399 = ( ( tex2DNode66.r + tex2DNode66.g + tex2DNode66.b ) / 3.0 );
				float temp_output_17_0_g12 = ( lerpResult565 * Texture_Value399 );
				float temp_output_19_0_g12 = _C1EndLerp;
				float4 temp_output_23_0_g12 = _Color2;
				float4 lerpResult11_g12 = lerp( _Color1 , temp_output_23_0_g12 , saturate( (0.0 + (temp_output_17_0_g12 - _C1StartLerp) * (1.0 - 0.0) / (temp_output_19_0_g12 - _C1StartLerp)) ));
				float4 lerpResult6_g12 = lerp( temp_output_23_0_g12 , _Color3 , saturate( (0.0 + (temp_output_17_0_g12 - _C2StartLerp) * (1.0 - 0.0) / (_C2EndLerp - _C2StartLerp)) ));
				float4 ColorRemapped408 = ( temp_output_17_0_g12 <= temp_output_19_0_g12 ? lerpResult11_g12 : lerpResult6_g12 );
				float lerpResult454 = lerp( _AlphaMultiStartFactor , _AlphaMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _AlphaMultiStartPosition) * (1.0 - 0.0) / (_AlphaMultiEndPosition - _AlphaMultiStartPosition)) ) , _AlphaMultiPower ));
				float AlphaMask204 = lerpResult454;
				

				float Alpha = ( ColorRemapped408.a * AlphaMask204 );
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					#ifdef _ALPHATEST_SHADOW_ON
						clip(Alpha - AlphaClipThresholdShadow);
					#else
						clip(Alpha - AlphaClipThreshold);
					#endif
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( input.positionCS );
				#endif

				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 170003


			#pragma vertex vert
			#pragma fragment frag

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				float4 clipPosV : TEXCOORD0;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 positionWS : TEXCOORD1;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color2;
			float4 _Color1;
			float _Scale;
			float _AlphaMultiStartPosition;
			float _AlphaMultiEndFactor;
			float _AlphaMultiStartFactor;
			float _C2EndLerp;
			float _C2StartLerp;
			float _C1StartLerp;
			float _C1EndLerp;
			float _RotateTiledTexture;
			int _PolarLength;
			float _TextureScale;
			float _TexCircularSpeed;
			float _TexRadialSpeed;
			int _UsePolarCoordinates;
			float _ValueMulitPower;
			float _ValueMultiEndPosition;
			float _ValueMultiStartPosition;
			float _ValueMultiEndFactor;
			float _ValueMultiStartFactor;
			float _MoveCenterDownPower;
			float _MoveCenterDown;
			float _WavesRotationsPerSec;
			int _Waves;
			float _WaveOffsetPerObject;
			float _WaveStrengthChangePower;
			float _WaveStrengthEdge;
			float _WaveStrengthCenter;
			float _AlphaMultiEndPosition;
			float _AlphaMultiPower;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _RingTexture;


			
			PackedVaryings VertexFunction( Attributes input  )
			{
				PackedVaryings output = (PackedVaryings)0;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float3 temp_output_193_0 = ( input.positionOS.xyz * _Scale );
				float3 Scaled_Vertices_Local303 = temp_output_193_0;
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult569 = lerp( _WaveStrengthCenter , _WaveStrengthEdge , pow( Distance_To_Center_01396 , _WaveStrengthChangePower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_489_0 = ( ase_objectScale.y * 10000.0 );
				float RandomY2492 = ( temp_output_489_0 - floor( temp_output_489_0 ) );
				float2 appendResult45 = (float2(input.positionOS.xyz.x , input.positionOS.xyz.z));
				float2 normalizeResult17 = normalize( appendResult45 );
				float2 break18 = normalizeResult17;
				float2 _Vector0 = float2(0,1);
				float dotResult19 = dot( normalizeResult17 , _Vector0 );
				float temp_output_16_0 = atan2( ( ( break18.x * _Vector0.y ) - ( break18.y * _Vector0.x ) ) , dotResult19 );
				float Angle_Atan161 = temp_output_16_0;
				float mulTime85 = _TimeParameters.x * ( _WavesRotationsPerSec * ( 2.0 * PI ) * _Waves );
				float temp_output_57_0 = sin( ( ( RandomY2492 * _WaveOffsetPerObject ) + ( ( Angle_Atan161 * _Waves ) + mulTime85 ) ) );
				float Vertical_Offset_For_Waves152 = ( lerpResult569 * temp_output_57_0 );
				float temp_output_591_0 = ( 1.0 - Distance_To_Center_01396 );
				float3 appendResult60 = (float3(0.0 , ( Vertical_Offset_For_Waves152 - ( temp_output_591_0 * pow( _MoveCenterDown , _MoveCenterDownPower ) ) ) , 0.0));
				
				output.ase_texcoord3 = input.positionOS;
				output.ase_texcoord4.xy = input.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord4.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ( Scaled_Vertices_Local303 + appendResult60 );

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				VertexPositionInputs vertexInput = GetVertexPositionInputs( input.positionOS.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					output.positionWS = vertexInput.positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					output.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				output.positionCS = vertexInput.positionCS;
				output.clipPosV = vertexInput.positionCS;
				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.vertex = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			half4 frag(PackedVaryings input  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( input );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = input.positionWS;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				float4 ClipPos = input.clipPosV;
				float4 ScreenPos = ComputeScreenPos( input.clipPosV );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = input.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float3 temp_output_193_0 = ( input.ase_texcoord3.xyz * _Scale );
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult565 = lerp( _ValueMultiStartFactor , _ValueMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _ValueMultiStartPosition) * (1.0 - 0.0) / (_ValueMultiEndPosition - _ValueMultiStartPosition)) ) , _ValueMulitPower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_485_0 = ( ase_objectScale.y * 1000.0 );
				float RandomY1488 = ( temp_output_485_0 - floor( temp_output_485_0 ) );
				float mulTime512 = _TimeParameters.x * _TexRadialSpeed;
				float mulTime507 = _TimeParameters.x * _TexCircularSpeed;
				float temp_output_339_0 = ( ase_objectScale.x * 1000.0 );
				float RandomX1341 = ( temp_output_339_0 - floor( temp_output_339_0 ) );
				float CircularSpeedTimedAndOffset509 = ( mulTime507 + ( RandomX1341 * 100.0 ) );
				float2 appendResult506 = (float2(( ( RandomY1488 * 100.0 ) + mulTime512 ) , CircularSpeedTimedAndOffset509));
				float TextureScale_PT516 = _TextureScale;
				float2 temp_output_34_0_g9 = ( input.ase_texcoord4.xy - float2( 0.5,0.5 ) );
				float2 break39_g9 = temp_output_34_0_g9;
				float2 appendResult50_g9 = (float2(( TextureScale_PT516 * ( length( temp_output_34_0_g9 ) * 2.0 ) ) , ( ( atan2( break39_g9.x , break39_g9.y ) * ( 1.0 / TWO_PI ) ) * (float)_PolarLength )));
				float cos585 = cos( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float sin585 = sin( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float2 rotator585 = mul( ( appendResult506 + appendResult50_g9 ) - float2( 0.5,0.5 ) , float2x2( cos585 , -sin585 , sin585 , cos585 )) + float2( 0.5,0.5 );
				float2 texCoord443 = input.ase_texcoord4.xy * float2( 1,1 ) + float2( 0,0 );
				float cos495 = cos( CircularSpeedTimedAndOffset509 );
				float sin495 = sin( CircularSpeedTimedAndOffset509 );
				float2 rotator495 = mul( ( ( ( texCoord443 - float2( 0.5,0.5 ) ) * ( 1.0 / TextureScale_PT516 ) ) + float2( 0.5,0.5 ) ) - float2( 0.5,0.5 ) , float2x2( cos495 , -sin495 , sin495 , cos495 )) + float2( 0.5,0.5 );
				float2 temp_cast_2 = (ddx( 0.0 )).xx;
				float2 temp_cast_3 = (ddy( 0.0 )).xx;
				float4 tex2DNode66 = tex2D( _RingTexture, ( (float)_UsePolarCoordinates >= 1.0 ? rotator585 : rotator495 ), temp_cast_2, temp_cast_3 );
				float Texture_Value399 = ( ( tex2DNode66.r + tex2DNode66.g + tex2DNode66.b ) / 3.0 );
				float temp_output_17_0_g12 = ( lerpResult565 * Texture_Value399 );
				float temp_output_19_0_g12 = _C1EndLerp;
				float4 temp_output_23_0_g12 = _Color2;
				float4 lerpResult11_g12 = lerp( _Color1 , temp_output_23_0_g12 , saturate( (0.0 + (temp_output_17_0_g12 - _C1StartLerp) * (1.0 - 0.0) / (temp_output_19_0_g12 - _C1StartLerp)) ));
				float4 lerpResult6_g12 = lerp( temp_output_23_0_g12 , _Color3 , saturate( (0.0 + (temp_output_17_0_g12 - _C2StartLerp) * (1.0 - 0.0) / (_C2EndLerp - _C2StartLerp)) ));
				float4 ColorRemapped408 = ( temp_output_17_0_g12 <= temp_output_19_0_g12 ? lerpResult11_g12 : lerpResult6_g12 );
				float lerpResult454 = lerp( _AlphaMultiStartFactor , _AlphaMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _AlphaMultiStartPosition) * (1.0 - 0.0) / (_AlphaMultiEndPosition - _AlphaMultiStartPosition)) ) , _AlphaMultiPower ));
				float AlphaMask204 = lerpResult454;
				

				float Alpha = ( ColorRemapped408.a * AlphaMask204 );
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( input.positionCS );
				#endif
				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "SceneSelectionPass"
			Tags { "LightMode"="SceneSelectionPass" }

			Cull Off
			AlphaToMask Off

			HLSLPROGRAM

			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 170003


			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color2;
			float4 _Color1;
			float _Scale;
			float _AlphaMultiStartPosition;
			float _AlphaMultiEndFactor;
			float _AlphaMultiStartFactor;
			float _C2EndLerp;
			float _C2StartLerp;
			float _C1StartLerp;
			float _C1EndLerp;
			float _RotateTiledTexture;
			int _PolarLength;
			float _TextureScale;
			float _TexCircularSpeed;
			float _TexRadialSpeed;
			int _UsePolarCoordinates;
			float _ValueMulitPower;
			float _ValueMultiEndPosition;
			float _ValueMultiStartPosition;
			float _ValueMultiEndFactor;
			float _ValueMultiStartFactor;
			float _MoveCenterDownPower;
			float _MoveCenterDown;
			float _WavesRotationsPerSec;
			int _Waves;
			float _WaveOffsetPerObject;
			float _WaveStrengthChangePower;
			float _WaveStrengthEdge;
			float _WaveStrengthCenter;
			float _AlphaMultiEndPosition;
			float _AlphaMultiPower;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _RingTexture;


			
			int _ObjectId;
			int _PassValue;

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			PackedVaryings VertexFunction(Attributes input  )
			{
				PackedVaryings output;
				ZERO_INITIALIZE(PackedVaryings, output);

				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float3 temp_output_193_0 = ( input.positionOS.xyz * _Scale );
				float3 Scaled_Vertices_Local303 = temp_output_193_0;
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult569 = lerp( _WaveStrengthCenter , _WaveStrengthEdge , pow( Distance_To_Center_01396 , _WaveStrengthChangePower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_489_0 = ( ase_objectScale.y * 10000.0 );
				float RandomY2492 = ( temp_output_489_0 - floor( temp_output_489_0 ) );
				float2 appendResult45 = (float2(input.positionOS.xyz.x , input.positionOS.xyz.z));
				float2 normalizeResult17 = normalize( appendResult45 );
				float2 break18 = normalizeResult17;
				float2 _Vector0 = float2(0,1);
				float dotResult19 = dot( normalizeResult17 , _Vector0 );
				float temp_output_16_0 = atan2( ( ( break18.x * _Vector0.y ) - ( break18.y * _Vector0.x ) ) , dotResult19 );
				float Angle_Atan161 = temp_output_16_0;
				float mulTime85 = _TimeParameters.x * ( _WavesRotationsPerSec * ( 2.0 * PI ) * _Waves );
				float temp_output_57_0 = sin( ( ( RandomY2492 * _WaveOffsetPerObject ) + ( ( Angle_Atan161 * _Waves ) + mulTime85 ) ) );
				float Vertical_Offset_For_Waves152 = ( lerpResult569 * temp_output_57_0 );
				float temp_output_591_0 = ( 1.0 - Distance_To_Center_01396 );
				float3 appendResult60 = (float3(0.0 , ( Vertical_Offset_For_Waves152 - ( temp_output_591_0 * pow( _MoveCenterDown , _MoveCenterDownPower ) ) ) , 0.0));
				
				output.ase_texcoord = input.positionOS;
				output.ase_texcoord1.xy = input.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord1.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ( Scaled_Vertices_Local303 + appendResult60 );

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				float3 positionWS = TransformObjectToWorld( input.positionOS.xyz );

				output.positionCS = TransformWorldToHClip(positionWS);

				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.vertex = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			half4 frag(PackedVaryings input ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float3 temp_output_193_0 = ( input.ase_texcoord.xyz * _Scale );
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult565 = lerp( _ValueMultiStartFactor , _ValueMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _ValueMultiStartPosition) * (1.0 - 0.0) / (_ValueMultiEndPosition - _ValueMultiStartPosition)) ) , _ValueMulitPower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_485_0 = ( ase_objectScale.y * 1000.0 );
				float RandomY1488 = ( temp_output_485_0 - floor( temp_output_485_0 ) );
				float mulTime512 = _TimeParameters.x * _TexRadialSpeed;
				float mulTime507 = _TimeParameters.x * _TexCircularSpeed;
				float temp_output_339_0 = ( ase_objectScale.x * 1000.0 );
				float RandomX1341 = ( temp_output_339_0 - floor( temp_output_339_0 ) );
				float CircularSpeedTimedAndOffset509 = ( mulTime507 + ( RandomX1341 * 100.0 ) );
				float2 appendResult506 = (float2(( ( RandomY1488 * 100.0 ) + mulTime512 ) , CircularSpeedTimedAndOffset509));
				float TextureScale_PT516 = _TextureScale;
				float2 temp_output_34_0_g9 = ( input.ase_texcoord1.xy - float2( 0.5,0.5 ) );
				float2 break39_g9 = temp_output_34_0_g9;
				float2 appendResult50_g9 = (float2(( TextureScale_PT516 * ( length( temp_output_34_0_g9 ) * 2.0 ) ) , ( ( atan2( break39_g9.x , break39_g9.y ) * ( 1.0 / TWO_PI ) ) * (float)_PolarLength )));
				float cos585 = cos( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float sin585 = sin( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float2 rotator585 = mul( ( appendResult506 + appendResult50_g9 ) - float2( 0.5,0.5 ) , float2x2( cos585 , -sin585 , sin585 , cos585 )) + float2( 0.5,0.5 );
				float2 texCoord443 = input.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float cos495 = cos( CircularSpeedTimedAndOffset509 );
				float sin495 = sin( CircularSpeedTimedAndOffset509 );
				float2 rotator495 = mul( ( ( ( texCoord443 - float2( 0.5,0.5 ) ) * ( 1.0 / TextureScale_PT516 ) ) + float2( 0.5,0.5 ) ) - float2( 0.5,0.5 ) , float2x2( cos495 , -sin495 , sin495 , cos495 )) + float2( 0.5,0.5 );
				float2 temp_cast_2 = (ddx( 0.0 )).xx;
				float2 temp_cast_3 = (ddy( 0.0 )).xx;
				float4 tex2DNode66 = tex2D( _RingTexture, ( (float)_UsePolarCoordinates >= 1.0 ? rotator585 : rotator495 ), temp_cast_2, temp_cast_3 );
				float Texture_Value399 = ( ( tex2DNode66.r + tex2DNode66.g + tex2DNode66.b ) / 3.0 );
				float temp_output_17_0_g12 = ( lerpResult565 * Texture_Value399 );
				float temp_output_19_0_g12 = _C1EndLerp;
				float4 temp_output_23_0_g12 = _Color2;
				float4 lerpResult11_g12 = lerp( _Color1 , temp_output_23_0_g12 , saturate( (0.0 + (temp_output_17_0_g12 - _C1StartLerp) * (1.0 - 0.0) / (temp_output_19_0_g12 - _C1StartLerp)) ));
				float4 lerpResult6_g12 = lerp( temp_output_23_0_g12 , _Color3 , saturate( (0.0 + (temp_output_17_0_g12 - _C2StartLerp) * (1.0 - 0.0) / (_C2EndLerp - _C2StartLerp)) ));
				float4 ColorRemapped408 = ( temp_output_17_0_g12 <= temp_output_19_0_g12 ? lerpResult11_g12 : lerpResult6_g12 );
				float lerpResult454 = lerp( _AlphaMultiStartFactor , _AlphaMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _AlphaMultiStartPosition) * (1.0 - 0.0) / (_AlphaMultiEndPosition - _AlphaMultiStartPosition)) ) , _AlphaMultiPower ));
				float AlphaMask204 = lerpResult454;
				

				surfaceDescription.Alpha = ( ColorRemapped408.a * AlphaMask204 );
				surfaceDescription.AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					float alphaClipThreshold = 0.01f;
					#if ALPHA_CLIP_THRESHOLD
						alphaClipThreshold = surfaceDescription.AlphaClipThreshold;
					#endif
					clip(surfaceDescription.Alpha - alphaClipThreshold);
				#endif

				half4 outColor = half4(_ObjectId, _PassValue, 1.0, 1.0);
				return outColor;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "ScenePickingPass"
			Tags { "LightMode"="Picking" }

			AlphaToMask Off

			HLSLPROGRAM

			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 170003


			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT

			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color2;
			float4 _Color1;
			float _Scale;
			float _AlphaMultiStartPosition;
			float _AlphaMultiEndFactor;
			float _AlphaMultiStartFactor;
			float _C2EndLerp;
			float _C2StartLerp;
			float _C1StartLerp;
			float _C1EndLerp;
			float _RotateTiledTexture;
			int _PolarLength;
			float _TextureScale;
			float _TexCircularSpeed;
			float _TexRadialSpeed;
			int _UsePolarCoordinates;
			float _ValueMulitPower;
			float _ValueMultiEndPosition;
			float _ValueMultiStartPosition;
			float _ValueMultiEndFactor;
			float _ValueMultiStartFactor;
			float _MoveCenterDownPower;
			float _MoveCenterDown;
			float _WavesRotationsPerSec;
			int _Waves;
			float _WaveOffsetPerObject;
			float _WaveStrengthChangePower;
			float _WaveStrengthEdge;
			float _WaveStrengthCenter;
			float _AlphaMultiEndPosition;
			float _AlphaMultiPower;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _RingTexture;


			
			float4 _SelectionID;

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			PackedVaryings VertexFunction(Attributes input  )
			{
				PackedVaryings output;
				ZERO_INITIALIZE(PackedVaryings, output);

				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float3 temp_output_193_0 = ( input.positionOS.xyz * _Scale );
				float3 Scaled_Vertices_Local303 = temp_output_193_0;
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult569 = lerp( _WaveStrengthCenter , _WaveStrengthEdge , pow( Distance_To_Center_01396 , _WaveStrengthChangePower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_489_0 = ( ase_objectScale.y * 10000.0 );
				float RandomY2492 = ( temp_output_489_0 - floor( temp_output_489_0 ) );
				float2 appendResult45 = (float2(input.positionOS.xyz.x , input.positionOS.xyz.z));
				float2 normalizeResult17 = normalize( appendResult45 );
				float2 break18 = normalizeResult17;
				float2 _Vector0 = float2(0,1);
				float dotResult19 = dot( normalizeResult17 , _Vector0 );
				float temp_output_16_0 = atan2( ( ( break18.x * _Vector0.y ) - ( break18.y * _Vector0.x ) ) , dotResult19 );
				float Angle_Atan161 = temp_output_16_0;
				float mulTime85 = _TimeParameters.x * ( _WavesRotationsPerSec * ( 2.0 * PI ) * _Waves );
				float temp_output_57_0 = sin( ( ( RandomY2492 * _WaveOffsetPerObject ) + ( ( Angle_Atan161 * _Waves ) + mulTime85 ) ) );
				float Vertical_Offset_For_Waves152 = ( lerpResult569 * temp_output_57_0 );
				float temp_output_591_0 = ( 1.0 - Distance_To_Center_01396 );
				float3 appendResult60 = (float3(0.0 , ( Vertical_Offset_For_Waves152 - ( temp_output_591_0 * pow( _MoveCenterDown , _MoveCenterDownPower ) ) ) , 0.0));
				
				output.ase_texcoord = input.positionOS;
				output.ase_texcoord1.xy = input.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord1.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ( Scaled_Vertices_Local303 + appendResult60 );

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				float3 positionWS = TransformObjectToWorld( input.positionOS.xyz );
				output.positionCS = TransformWorldToHClip(positionWS);
				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.vertex = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			half4 frag(PackedVaryings input ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float3 temp_output_193_0 = ( input.ase_texcoord.xyz * _Scale );
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult565 = lerp( _ValueMultiStartFactor , _ValueMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _ValueMultiStartPosition) * (1.0 - 0.0) / (_ValueMultiEndPosition - _ValueMultiStartPosition)) ) , _ValueMulitPower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_485_0 = ( ase_objectScale.y * 1000.0 );
				float RandomY1488 = ( temp_output_485_0 - floor( temp_output_485_0 ) );
				float mulTime512 = _TimeParameters.x * _TexRadialSpeed;
				float mulTime507 = _TimeParameters.x * _TexCircularSpeed;
				float temp_output_339_0 = ( ase_objectScale.x * 1000.0 );
				float RandomX1341 = ( temp_output_339_0 - floor( temp_output_339_0 ) );
				float CircularSpeedTimedAndOffset509 = ( mulTime507 + ( RandomX1341 * 100.0 ) );
				float2 appendResult506 = (float2(( ( RandomY1488 * 100.0 ) + mulTime512 ) , CircularSpeedTimedAndOffset509));
				float TextureScale_PT516 = _TextureScale;
				float2 temp_output_34_0_g9 = ( input.ase_texcoord1.xy - float2( 0.5,0.5 ) );
				float2 break39_g9 = temp_output_34_0_g9;
				float2 appendResult50_g9 = (float2(( TextureScale_PT516 * ( length( temp_output_34_0_g9 ) * 2.0 ) ) , ( ( atan2( break39_g9.x , break39_g9.y ) * ( 1.0 / TWO_PI ) ) * (float)_PolarLength )));
				float cos585 = cos( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float sin585 = sin( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float2 rotator585 = mul( ( appendResult506 + appendResult50_g9 ) - float2( 0.5,0.5 ) , float2x2( cos585 , -sin585 , sin585 , cos585 )) + float2( 0.5,0.5 );
				float2 texCoord443 = input.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float cos495 = cos( CircularSpeedTimedAndOffset509 );
				float sin495 = sin( CircularSpeedTimedAndOffset509 );
				float2 rotator495 = mul( ( ( ( texCoord443 - float2( 0.5,0.5 ) ) * ( 1.0 / TextureScale_PT516 ) ) + float2( 0.5,0.5 ) ) - float2( 0.5,0.5 ) , float2x2( cos495 , -sin495 , sin495 , cos495 )) + float2( 0.5,0.5 );
				float2 temp_cast_2 = (ddx( 0.0 )).xx;
				float2 temp_cast_3 = (ddy( 0.0 )).xx;
				float4 tex2DNode66 = tex2D( _RingTexture, ( (float)_UsePolarCoordinates >= 1.0 ? rotator585 : rotator495 ), temp_cast_2, temp_cast_3 );
				float Texture_Value399 = ( ( tex2DNode66.r + tex2DNode66.g + tex2DNode66.b ) / 3.0 );
				float temp_output_17_0_g12 = ( lerpResult565 * Texture_Value399 );
				float temp_output_19_0_g12 = _C1EndLerp;
				float4 temp_output_23_0_g12 = _Color2;
				float4 lerpResult11_g12 = lerp( _Color1 , temp_output_23_0_g12 , saturate( (0.0 + (temp_output_17_0_g12 - _C1StartLerp) * (1.0 - 0.0) / (temp_output_19_0_g12 - _C1StartLerp)) ));
				float4 lerpResult6_g12 = lerp( temp_output_23_0_g12 , _Color3 , saturate( (0.0 + (temp_output_17_0_g12 - _C2StartLerp) * (1.0 - 0.0) / (_C2EndLerp - _C2StartLerp)) ));
				float4 ColorRemapped408 = ( temp_output_17_0_g12 <= temp_output_19_0_g12 ? lerpResult11_g12 : lerpResult6_g12 );
				float lerpResult454 = lerp( _AlphaMultiStartFactor , _AlphaMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _AlphaMultiStartPosition) * (1.0 - 0.0) / (_AlphaMultiEndPosition - _AlphaMultiStartPosition)) ) , _AlphaMultiPower ));
				float AlphaMask204 = lerpResult454;
				

				surfaceDescription.Alpha = ( ColorRemapped408.a * AlphaMask204 );
				surfaceDescription.AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					float alphaClipThreshold = 0.01f;
					#if ALPHA_CLIP_THRESHOLD
						alphaClipThreshold = surfaceDescription.AlphaClipThreshold;
					#endif
					clip(surfaceDescription.Alpha - alphaClipThreshold);
				#endif

				half4 outColor = 0;
				outColor = _SelectionID;

				return outColor;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthNormals"
			Tags { "LightMode"="DepthNormalsOnly" }

			ZTest LEqual
			ZWrite On

			HLSLPROGRAM

        	#pragma multi_compile_instancing
        	#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
        	#define ASE_FOG 1
        	#define ASE_ABSOLUTE_VERTEX_POS 1
        	#define _SURFACE_TYPE_TRANSPARENT 1
        	#define ASE_SRP_VERSION 170003


        	#pragma multi_compile_fragment _ _GBUFFER_NORMALS_OCT

			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define VARYINGS_NEED_NORMAL_WS

			#define SHADERPASS SHADERPASS_DEPTHNORMALSONLY

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

            #if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				float4 clipPosV : TEXCOORD0;
				float3 normalWS : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color2;
			float4 _Color1;
			float _Scale;
			float _AlphaMultiStartPosition;
			float _AlphaMultiEndFactor;
			float _AlphaMultiStartFactor;
			float _C2EndLerp;
			float _C2StartLerp;
			float _C1StartLerp;
			float _C1EndLerp;
			float _RotateTiledTexture;
			int _PolarLength;
			float _TextureScale;
			float _TexCircularSpeed;
			float _TexRadialSpeed;
			int _UsePolarCoordinates;
			float _ValueMulitPower;
			float _ValueMultiEndPosition;
			float _ValueMultiStartPosition;
			float _ValueMultiEndFactor;
			float _ValueMultiStartFactor;
			float _MoveCenterDownPower;
			float _MoveCenterDown;
			float _WavesRotationsPerSec;
			int _Waves;
			float _WaveOffsetPerObject;
			float _WaveStrengthChangePower;
			float _WaveStrengthEdge;
			float _WaveStrengthCenter;
			float _AlphaMultiEndPosition;
			float _AlphaMultiPower;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _RingTexture;


			
			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			PackedVaryings VertexFunction(Attributes input  )
			{
				PackedVaryings output;
				ZERO_INITIALIZE(PackedVaryings, output);

				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float3 temp_output_193_0 = ( input.positionOS.xyz * _Scale );
				float3 Scaled_Vertices_Local303 = temp_output_193_0;
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult569 = lerp( _WaveStrengthCenter , _WaveStrengthEdge , pow( Distance_To_Center_01396 , _WaveStrengthChangePower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_489_0 = ( ase_objectScale.y * 10000.0 );
				float RandomY2492 = ( temp_output_489_0 - floor( temp_output_489_0 ) );
				float2 appendResult45 = (float2(input.positionOS.xyz.x , input.positionOS.xyz.z));
				float2 normalizeResult17 = normalize( appendResult45 );
				float2 break18 = normalizeResult17;
				float2 _Vector0 = float2(0,1);
				float dotResult19 = dot( normalizeResult17 , _Vector0 );
				float temp_output_16_0 = atan2( ( ( break18.x * _Vector0.y ) - ( break18.y * _Vector0.x ) ) , dotResult19 );
				float Angle_Atan161 = temp_output_16_0;
				float mulTime85 = _TimeParameters.x * ( _WavesRotationsPerSec * ( 2.0 * PI ) * _Waves );
				float temp_output_57_0 = sin( ( ( RandomY2492 * _WaveOffsetPerObject ) + ( ( Angle_Atan161 * _Waves ) + mulTime85 ) ) );
				float Vertical_Offset_For_Waves152 = ( lerpResult569 * temp_output_57_0 );
				float temp_output_591_0 = ( 1.0 - Distance_To_Center_01396 );
				float3 appendResult60 = (float3(0.0 , ( Vertical_Offset_For_Waves152 - ( temp_output_591_0 * pow( _MoveCenterDown , _MoveCenterDownPower ) ) ) , 0.0));
				
				output.ase_texcoord2 = input.positionOS;
				output.ase_texcoord3.xy = input.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord3.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ( Scaled_Vertices_Local303 + appendResult60 );

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				input.normalOS = input.normalOS;

				VertexPositionInputs vertexInput = GetVertexPositionInputs( input.positionOS.xyz );

				output.positionCS = vertexInput.positionCS;
				output.clipPosV = vertexInput.positionCS;
				output.normalWS = TransformObjectToWorldNormal( input.normalOS );
				return output;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( Attributes input )
			{
				VertexControl output;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				output.vertex = input.positionOS;
				output.normalOS = input.normalOS;
				output.ase_texcoord = input.ase_texcoord;
				return output;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> input)
			{
				TessellationFactors output;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(input[0].vertex, input[1].vertex, input[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				output.edge[0] = tf.x; output.edge[1] = tf.y; output.edge[2] = tf.z; output.inside = tf.w;
				return output;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			PackedVaryings DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				Attributes output = (Attributes) 0;
				output.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				output.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				output.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = output.positionOS.xyz - patch[i].normalOS * (dot(output.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				output.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * output.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], output);
				return VertexFunction(output);
			}
			#else
			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}
			#endif

			void frag( PackedVaryings input
				, out half4 outNormalWS : SV_Target0
			#ifdef _WRITE_RENDERING_LAYERS
				, out float4 outRenderingLayers : SV_Target1
			#endif
				 )
			{
				float4 ClipPos = input.clipPosV;
				float4 ScreenPos = ComputeScreenPos( input.clipPosV );

				float3 temp_output_193_0 = ( input.ase_texcoord2.xyz * _Scale );
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult565 = lerp( _ValueMultiStartFactor , _ValueMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _ValueMultiStartPosition) * (1.0 - 0.0) / (_ValueMultiEndPosition - _ValueMultiStartPosition)) ) , _ValueMulitPower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_485_0 = ( ase_objectScale.y * 1000.0 );
				float RandomY1488 = ( temp_output_485_0 - floor( temp_output_485_0 ) );
				float mulTime512 = _TimeParameters.x * _TexRadialSpeed;
				float mulTime507 = _TimeParameters.x * _TexCircularSpeed;
				float temp_output_339_0 = ( ase_objectScale.x * 1000.0 );
				float RandomX1341 = ( temp_output_339_0 - floor( temp_output_339_0 ) );
				float CircularSpeedTimedAndOffset509 = ( mulTime507 + ( RandomX1341 * 100.0 ) );
				float2 appendResult506 = (float2(( ( RandomY1488 * 100.0 ) + mulTime512 ) , CircularSpeedTimedAndOffset509));
				float TextureScale_PT516 = _TextureScale;
				float2 temp_output_34_0_g9 = ( input.ase_texcoord3.xy - float2( 0.5,0.5 ) );
				float2 break39_g9 = temp_output_34_0_g9;
				float2 appendResult50_g9 = (float2(( TextureScale_PT516 * ( length( temp_output_34_0_g9 ) * 2.0 ) ) , ( ( atan2( break39_g9.x , break39_g9.y ) * ( 1.0 / TWO_PI ) ) * (float)_PolarLength )));
				float cos585 = cos( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float sin585 = sin( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float2 rotator585 = mul( ( appendResult506 + appendResult50_g9 ) - float2( 0.5,0.5 ) , float2x2( cos585 , -sin585 , sin585 , cos585 )) + float2( 0.5,0.5 );
				float2 texCoord443 = input.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float cos495 = cos( CircularSpeedTimedAndOffset509 );
				float sin495 = sin( CircularSpeedTimedAndOffset509 );
				float2 rotator495 = mul( ( ( ( texCoord443 - float2( 0.5,0.5 ) ) * ( 1.0 / TextureScale_PT516 ) ) + float2( 0.5,0.5 ) ) - float2( 0.5,0.5 ) , float2x2( cos495 , -sin495 , sin495 , cos495 )) + float2( 0.5,0.5 );
				float2 temp_cast_2 = (ddx( 0.0 )).xx;
				float2 temp_cast_3 = (ddy( 0.0 )).xx;
				float4 tex2DNode66 = tex2D( _RingTexture, ( (float)_UsePolarCoordinates >= 1.0 ? rotator585 : rotator495 ), temp_cast_2, temp_cast_3 );
				float Texture_Value399 = ( ( tex2DNode66.r + tex2DNode66.g + tex2DNode66.b ) / 3.0 );
				float temp_output_17_0_g12 = ( lerpResult565 * Texture_Value399 );
				float temp_output_19_0_g12 = _C1EndLerp;
				float4 temp_output_23_0_g12 = _Color2;
				float4 lerpResult11_g12 = lerp( _Color1 , temp_output_23_0_g12 , saturate( (0.0 + (temp_output_17_0_g12 - _C1StartLerp) * (1.0 - 0.0) / (temp_output_19_0_g12 - _C1StartLerp)) ));
				float4 lerpResult6_g12 = lerp( temp_output_23_0_g12 , _Color3 , saturate( (0.0 + (temp_output_17_0_g12 - _C2StartLerp) * (1.0 - 0.0) / (_C2EndLerp - _C2StartLerp)) ));
				float4 ColorRemapped408 = ( temp_output_17_0_g12 <= temp_output_19_0_g12 ? lerpResult11_g12 : lerpResult6_g12 );
				float lerpResult454 = lerp( _AlphaMultiStartFactor , _AlphaMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _AlphaMultiStartPosition) * (1.0 - 0.0) / (_AlphaMultiEndPosition - _AlphaMultiStartPosition)) ) , _AlphaMultiPower ));
				float AlphaMask204 = lerpResult454;
				

				float Alpha = ( ColorRemapped408.a * AlphaMask204 );
				float AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					clip( Alpha - AlphaClipThreshold );
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( input.positionCS );
				#endif

				#if defined(_GBUFFER_NORMALS_OCT)
					float3 normalWS = normalize(input.normalWS);
					float2 octNormalWS = PackNormalOctQuadEncode(normalWS);           // values between [-1, +1], must use fp32 on some platforms
					float2 remappedOctNormalWS = saturate(octNormalWS * 0.5 + 0.5);   // values between [ 0,  1]
					half3 packedNormalWS = PackFloat2To888(remappedOctNormalWS);      // values between [ 0,  1]
					outNormalWS = half4(packedNormalWS, 0.0);
				#else
					float3 normalWS = input.normalWS;
					outNormalWS = half4(NormalizeNormalPerPixel(normalWS), 0.0);
				#endif

				#ifdef _WRITE_RENDERING_LAYERS
					uint renderingLayers = GetMeshRenderingLayer();
					outRenderingLayers = float4(EncodeMeshRenderingLayer(renderingLayers), 0, 0, 0);
				#endif
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "MotionVectors"
			Tags { "LightMode"="MotionVectors" }

			ColorMask RG

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 170003


			#pragma vertex vert
			#pragma fragment frag

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
		    #include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
		    #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
		    #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Input.hlsl"
		    #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include_with_pragmas "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRenderingKeywords.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/FoveatedRendering.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
		    #include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MotionVectorsCommon.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct Attributes
			{
				float4 positionOS : POSITION;
				float3 positionOld : TEXCOORD4;
				#if _ADD_PRECOMPUTED_VELOCITY
					float3 alembicMotionVector : TEXCOORD5;
				#endif
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct PackedVaryings
			{
				float4 positionCS : SV_POSITION;
				float4 positionCSNoJitter : TEXCOORD0;
				float4 previousPositionCSNoJitter : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color2;
			float4 _Color1;
			float _Scale;
			float _AlphaMultiStartPosition;
			float _AlphaMultiEndFactor;
			float _AlphaMultiStartFactor;
			float _C2EndLerp;
			float _C2StartLerp;
			float _C1StartLerp;
			float _C1EndLerp;
			float _RotateTiledTexture;
			int _PolarLength;
			float _TextureScale;
			float _TexCircularSpeed;
			float _TexRadialSpeed;
			int _UsePolarCoordinates;
			float _ValueMulitPower;
			float _ValueMultiEndPosition;
			float _ValueMultiStartPosition;
			float _ValueMultiEndFactor;
			float _ValueMultiStartFactor;
			float _MoveCenterDownPower;
			float _MoveCenterDown;
			float _WavesRotationsPerSec;
			int _Waves;
			float _WaveOffsetPerObject;
			float _WaveStrengthChangePower;
			float _WaveStrengthEdge;
			float _WaveStrengthCenter;
			float _AlphaMultiEndPosition;
			float _AlphaMultiPower;
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			#ifdef SCENEPICKINGPASS
				float4 _SelectionID;
			#endif

			#ifdef SCENESELECTIONPASS
				int _ObjectId;
				int _PassValue;
			#endif

			sampler2D _RingTexture;


			
			PackedVaryings VertexFunction( Attributes input  )
			{
				PackedVaryings output = (PackedVaryings)0;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, output);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);

				float3 temp_output_193_0 = ( input.positionOS.xyz * _Scale );
				float3 Scaled_Vertices_Local303 = temp_output_193_0;
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult569 = lerp( _WaveStrengthCenter , _WaveStrengthEdge , pow( Distance_To_Center_01396 , _WaveStrengthChangePower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_489_0 = ( ase_objectScale.y * 10000.0 );
				float RandomY2492 = ( temp_output_489_0 - floor( temp_output_489_0 ) );
				float2 appendResult45 = (float2(input.positionOS.xyz.x , input.positionOS.xyz.z));
				float2 normalizeResult17 = normalize( appendResult45 );
				float2 break18 = normalizeResult17;
				float2 _Vector0 = float2(0,1);
				float dotResult19 = dot( normalizeResult17 , _Vector0 );
				float temp_output_16_0 = atan2( ( ( break18.x * _Vector0.y ) - ( break18.y * _Vector0.x ) ) , dotResult19 );
				float Angle_Atan161 = temp_output_16_0;
				float mulTime85 = _TimeParameters.x * ( _WavesRotationsPerSec * ( 2.0 * PI ) * _Waves );
				float temp_output_57_0 = sin( ( ( RandomY2492 * _WaveOffsetPerObject ) + ( ( Angle_Atan161 * _Waves ) + mulTime85 ) ) );
				float Vertical_Offset_For_Waves152 = ( lerpResult569 * temp_output_57_0 );
				float temp_output_591_0 = ( 1.0 - Distance_To_Center_01396 );
				float3 appendResult60 = (float3(0.0 , ( Vertical_Offset_For_Waves152 - ( temp_output_591_0 * pow( _MoveCenterDown , _MoveCenterDownPower ) ) ) , 0.0));
				
				output.ase_texcoord2 = input.positionOS;
				output.ase_texcoord3.xy = input.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				output.ase_texcoord3.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = input.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = ( Scaled_Vertices_Local303 + appendResult60 );

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					input.positionOS.xyz = vertexValue;
				#else
					input.positionOS.xyz += vertexValue;
				#endif

				VertexPositionInputs vertexInput = GetVertexPositionInputs( input.positionOS.xyz );

				// Jittered. Match the frame.
				output.positionCS = vertexInput.positionCS;
				output.positionCSNoJitter = mul( _NonJitteredViewProjMatrix, mul( UNITY_MATRIX_M, input.positionOS ) );

				float4 prevPos = ( unity_MotionVectorsParams.x == 1 ) ? float4( input.positionOld, 1 ) : input.positionOS;

				#if _ADD_PRECOMPUTED_VELOCITY
					prevPos = prevPos - float4(input.alembicMotionVector, 0);
				#endif

				output.previousPositionCSNoJitter = mul( _PrevViewProjMatrix, mul( UNITY_PREV_MATRIX_M, prevPos ) );

				ApplyMotionVectorZBias( output.positionCS );
				return output;
			}

			PackedVaryings vert ( Attributes input )
			{
				return VertexFunction( input );
			}

			half4 frag(	PackedVaryings input  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( input );

				float3 temp_output_193_0 = ( input.ase_texcoord2.xyz * _Scale );
				float Distance_To_Center_01396 = ( length( temp_output_193_0 ) / _Scale );
				float lerpResult565 = lerp( _ValueMultiStartFactor , _ValueMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _ValueMultiStartPosition) * (1.0 - 0.0) / (_ValueMultiEndPosition - _ValueMultiStartPosition)) ) , _ValueMulitPower ));
				float3 ase_objectScale = float3( length( GetObjectToWorldMatrix()[ 0 ].xyz ), length( GetObjectToWorldMatrix()[ 1 ].xyz ), length( GetObjectToWorldMatrix()[ 2 ].xyz ) );
				float temp_output_485_0 = ( ase_objectScale.y * 1000.0 );
				float RandomY1488 = ( temp_output_485_0 - floor( temp_output_485_0 ) );
				float mulTime512 = _TimeParameters.x * _TexRadialSpeed;
				float mulTime507 = _TimeParameters.x * _TexCircularSpeed;
				float temp_output_339_0 = ( ase_objectScale.x * 1000.0 );
				float RandomX1341 = ( temp_output_339_0 - floor( temp_output_339_0 ) );
				float CircularSpeedTimedAndOffset509 = ( mulTime507 + ( RandomX1341 * 100.0 ) );
				float2 appendResult506 = (float2(( ( RandomY1488 * 100.0 ) + mulTime512 ) , CircularSpeedTimedAndOffset509));
				float TextureScale_PT516 = _TextureScale;
				float2 temp_output_34_0_g9 = ( input.ase_texcoord3.xy - float2( 0.5,0.5 ) );
				float2 break39_g9 = temp_output_34_0_g9;
				float2 appendResult50_g9 = (float2(( TextureScale_PT516 * ( length( temp_output_34_0_g9 ) * 2.0 ) ) , ( ( atan2( break39_g9.x , break39_g9.y ) * ( 1.0 / TWO_PI ) ) * (float)_PolarLength )));
				float cos585 = cos( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float sin585 = sin( ( ( _RotateTiledTexture / 2.0 ) * PI ) );
				float2 rotator585 = mul( ( appendResult506 + appendResult50_g9 ) - float2( 0.5,0.5 ) , float2x2( cos585 , -sin585 , sin585 , cos585 )) + float2( 0.5,0.5 );
				float2 texCoord443 = input.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float cos495 = cos( CircularSpeedTimedAndOffset509 );
				float sin495 = sin( CircularSpeedTimedAndOffset509 );
				float2 rotator495 = mul( ( ( ( texCoord443 - float2( 0.5,0.5 ) ) * ( 1.0 / TextureScale_PT516 ) ) + float2( 0.5,0.5 ) ) - float2( 0.5,0.5 ) , float2x2( cos495 , -sin495 , sin495 , cos495 )) + float2( 0.5,0.5 );
				float2 temp_cast_2 = (ddx( 0.0 )).xx;
				float2 temp_cast_3 = (ddy( 0.0 )).xx;
				float4 tex2DNode66 = tex2D( _RingTexture, ( (float)_UsePolarCoordinates >= 1.0 ? rotator585 : rotator495 ), temp_cast_2, temp_cast_3 );
				float Texture_Value399 = ( ( tex2DNode66.r + tex2DNode66.g + tex2DNode66.b ) / 3.0 );
				float temp_output_17_0_g12 = ( lerpResult565 * Texture_Value399 );
				float temp_output_19_0_g12 = _C1EndLerp;
				float4 temp_output_23_0_g12 = _Color2;
				float4 lerpResult11_g12 = lerp( _Color1 , temp_output_23_0_g12 , saturate( (0.0 + (temp_output_17_0_g12 - _C1StartLerp) * (1.0 - 0.0) / (temp_output_19_0_g12 - _C1StartLerp)) ));
				float4 lerpResult6_g12 = lerp( temp_output_23_0_g12 , _Color3 , saturate( (0.0 + (temp_output_17_0_g12 - _C2StartLerp) * (1.0 - 0.0) / (_C2EndLerp - _C2StartLerp)) ));
				float4 ColorRemapped408 = ( temp_output_17_0_g12 <= temp_output_19_0_g12 ? lerpResult11_g12 : lerpResult6_g12 );
				float lerpResult454 = lerp( _AlphaMultiStartFactor , _AlphaMultiEndFactor , pow( saturate( (0.0 + (Distance_To_Center_01396 - _AlphaMultiStartPosition) * (1.0 - 0.0) / (_AlphaMultiEndPosition - _AlphaMultiStartPosition)) ) , _AlphaMultiPower ));
				float AlphaMask204 = lerpResult454;
				

				float Alpha = ( ColorRemapped408.a * AlphaMask204 );
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( input.positionCS );
				#endif

				return float4( CalcNdcMotionVectorFromCsPositions( input.positionCSNoJitter, input.previousPositionCSNoJitter ), 0, 0 );
			}
			ENDHLSL
		}
		
	}
	
	CustomEditor "UnityEditor.ShaderGraphUnlitGUI"
	FallBack "Hidden/Shader Graph/FallbackError"
	
	Fallback Off
}
/*ASEBEGIN
Version=19603
Node;AmplifyShaderEditor.CommentaryNode;480;-2800,-5440;Inherit;False;1558.047;1058.886;;19;341;337;340;339;479;481;482;483;484;485;486;487;488;489;490;491;492;553;599;Random Seeds;0.1333214,0.2884097,0.2000622,1;0;0
Node;AmplifyShaderEditor.ObjectScaleNode;599;-2778.63,-5122.782;Inherit;False;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;339;-2336,-5392;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1000;False;1;FLOAT;0
Node;AmplifyShaderEditor.FloorOpNode;337;-2176,-5264;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;485;-2400,-4880;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1000;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;340;-2000,-5392;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;521;-4624,-2496;Inherit;False;1324.725;488.4646;;8;509;507;516;500;468;522;524;526;Texture Speed;0.09995236,0.07334653,0.1644204,1;0;0
Node;AmplifyShaderEditor.FloorOpNode;487;-2272,-4736;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;341;-1760,-5392;Inherit;False;RandomX1;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;486;-2064,-4880;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;522;-4416,-2336;Inherit;False;341;RandomX1;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;54;-3600,-720;Inherit;False;2206.203;655.4427;;15;32;20;21;27;161;16;19;13;14;15;18;336;17;45;368;Angle;0.1156715,0.03179066,0.1773585,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;500;-4512,-2432;Inherit;False;Property;_TexCircularSpeed;TexCircularSpeed;4;0;Create;True;0;0;0;False;0;False;0;-0.12;-24;24;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;468;-4384,-2176;Inherit;False;Property;_TextureScale;TextureScale;3;0;Create;True;0;0;0;False;0;False;1;1;0;4;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;515;-4592,-3440;Inherit;False;1465.24;666.3463;;12;520;505;363;506;470;518;512;511;499;525;527;523;Polar Coordinate for Tiled Textures;0.0360321,0.174492,0.2938005,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;488;-1840,-4880;Inherit;False;RandomY1;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;526;-4176,-2336;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;507;-4208,-2432;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;368;-3552,-544;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;514;-4704,-1776;Inherit;False;1512.898;445.2793;;9;495;510;476;473;477;471;443;517;519;Normal UV for Ring Textures;0.3315364,0.02457475,0.2389247,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;516;-4080,-2176;Inherit;False;TextureScale PT;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;524;-3984,-2400;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;523;-4288,-3376;Inherit;False;488;RandomY1;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;45;-3328,-496;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;499;-4336,-3264;Inherit;False;Property;_TexRadialSpeed;TexRadialSpeed;6;0;Create;True;0;0;0;False;0;False;1;0;-10;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;443;-4576,-1712;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;517;-4288,-1520;Inherit;False;516;TextureScale PT;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;509;-3840,-2432;Inherit;False;CircularSpeedTimedAndOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;512;-4032,-3264;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;527;-4000,-3376;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalizeNode;17;-3120,-496;Inherit;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;302;-3632,112;Inherit;False;1690.131;491.5171;;8;209;193;189;191;303;395;396;397;Scale;0.1057316,0.3027535,0.5603775,1;0;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;471;-4080,-1696;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;477;-4032,-1536;Inherit;False;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;518;-4048,-2992;Inherit;False;516;TextureScale PT;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;470;-4016,-2896;Inherit;False;Property;_PolarLength;Polar Length;7;0;Create;True;0;0;0;False;0;False;1;1;False;0;1;INT;0
Node;AmplifyShaderEditor.GetLocalVarNode;511;-4048,-3136;Inherit;False;509;CircularSpeedTimedAndOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;525;-3808,-3280;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;586;-3920,-2784;Inherit;False;Property;_RotateTiledTexture;RotateTiledTexture;2;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;18;-2880,-608;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.Vector2Node;336;-2960,-336;Inherit;False;Constant;_Vector0;Vector 0;19;0;Create;True;0;0;0;False;0;False;0,1;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.PosVertexDataNode;189;-3504,160;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;191;-3600,368;Inherit;False;Property;_Scale;Scale;0;0;Create;True;0;0;0;False;0;False;1;2.26;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;473;-3856,-1712;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;363;-3728,-3024;Inherit;False;Polar Coordinates;-1;;9;7dab8e02884cf104ebefaa2e788e4162;0;4;1;FLOAT2;0,0;False;2;FLOAT2;0.5,0.5;False;3;FLOAT;1;False;4;FLOAT;1;False;3;FLOAT2;0;FLOAT;55;FLOAT;56
Node;AmplifyShaderEditor.DynamicAppendNode;506;-3664,-3200;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;588;-3632,-2704;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;15;-2608,-656;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;14;-2608,-544;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;193;-3264,224;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;145;-3088,-2544;Inherit;False;1426.071;563.5663;;7;399;68;67;66;445;444;597;Ring Texture Value;0.4558785,0.4248488,0.6622642,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;476;-3632,-1712;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;510;-3824,-1456;Inherit;False;509;CircularSpeedTimedAndOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;505;-3392,-3136;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.PiNode;587;-3472,-2816;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;13;-2400,-592;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;19;-2624,-368;Inherit;True;2;0;FLOAT2;0,1;False;1;FLOAT2;0,1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;489;-2416,-4640;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;10000;False;1;FLOAT;0
Node;AmplifyShaderEditor.LengthOpNode;395;-3008,336;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;444;-3216,-2288;Inherit;False;Property;_UsePolarCoordinates;UsePolarCoordinates;5;0;Create;True;0;0;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.RotatorNode;495;-3456,-1616;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;2;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RotatorNode;585;-3248,-2928;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;2;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ATan2OpNode;16;-2192,-496;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;151;-1216,-624;Inherit;False;2297.524;574.0305;;12;57;83;85;62;52;163;84;162;63;61;583;548;Waves;0.4075473,0.1691706,0.2769719,1;0;0
Node;AmplifyShaderEditor.FloorOpNode;491;-2288,-4496;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;397;-2752,400;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;445;-2832,-2256;Inherit;False;3;4;0;INT;0;False;1;FLOAT;1;False;2;FLOAT2;0,0;False;3;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DdyOpNode;598;-2731.467,-1990.452;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DdxOpNode;597;-2816,-2080;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;161;-1952,-592;Inherit;False;Angle Atan;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;162;-1104,-240;Inherit;False;1;0;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;84;-1184,-336;Inherit;False;Property;_WavesRotationsPerSec;WavesRotationsPerSec;12;0;Create;True;0;0;0;False;0;False;0;0.199;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;63;-1072,-496;Inherit;False;Property;_Waves;Waves;8;0;Create;True;0;0;0;False;0;False;0;5;False;0;1;INT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;490;-2080,-4640;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;396;-2560,384;Inherit;False;Distance To Center 01;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;66;-2656,-2272;Inherit;True;Property;_RingTexture;RingTexture;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Derivative;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.GetLocalVarNode;52;-1072,-576;Inherit;False;161;Angle Atan;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;163;-832,-272;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;INT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;492;-1856,-4640;Inherit;False;RandomY2;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;462;592,-5616;Inherit;False;396;Distance To Center 01;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;465;576,-5408;Inherit;False;Property;_ValueMultiEndPosition;ValueMultiEndPosition;19;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;464;576,-5504;Inherit;False;Property;_ValueMultiStartPosition;ValueMultiStartPosition;17;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;67;-2320,-2224;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;62;-816,-480;Inherit;False;2;2;0;FLOAT;0;False;1;INT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;85;-656,-272;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;550;-896,-720;Inherit;False;Property;_WaveOffsetPerObject;WaveOffsetPerObject;13;0;Create;True;0;0;0;False;0;False;0;4.6;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;549;-800,-800;Inherit;False;492;RandomY2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;559;896,-5552;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;68;-2176,-2224;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;570;64,-864;Inherit;False;396;Distance To Center 01;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;578;16,-768;Inherit;False;Property;_WaveStrengthChangePower;WaveStrengthChangePower;11;0;Create;True;0;0;0;False;0;False;1;1;0.01;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;547;-528,-752;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;83;-528,-464;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;560;1120,-5424;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;466;1008,-5264;Inherit;False;Property;_ValueMulitPower;ValueMulitPower;21;0;Create;True;0;0;0;False;0;False;1;1;0.01;30;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;150;1280,-1728;Inherit;False;1963.478;581.8386;;11;204;454;452;451;187;186;90;87;467;88;228;Visibility;0.3728268,0.2222286,0.4075473,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;399;-2016,-2224;Inherit;False;Texture Value;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;556;96,-1056;Inherit;False;Property;_WaveStrengthCenter;WaveStrengthCenter;9;0;Create;True;0;0;0;False;0;False;0;0.41;0;4;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;56;96,-960;Inherit;False;Property;_WaveStrengthEdge;WaveStrengthEdge;10;0;Create;True;0;0;0;False;0;False;0;0;0;4;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;577;352,-768;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;548;-352,-512;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;552;528,-5024;Inherit;False;1636;1466.7;;10;147;79;355;356;358;359;360;357;408;589;Color;0,0.09425113,0.2237196,1;0;0
Node;AmplifyShaderEditor.PowerNode;564;1440,-5296;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;562;1264,-5616;Inherit;False;Property;_ValueMultiStartFactor;ValueMultiStartFactor;18;0;Create;True;0;0;0;False;0;False;1;1;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;563;1264,-5504;Inherit;False;Property;_ValueMultiEndFactor;ValueMultiEndFactor;20;0;Create;True;0;0;0;False;0;False;0;0.59;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;228;1632,-1680;Inherit;False;396;Distance To Center 01;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;88;1600,-1568;Inherit;False;Property;_AlphaMultiStartPosition;AlphaMultiStartPosition;22;0;Create;True;0;0;0;False;0;False;0;0.299;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;467;1632,-1488;Inherit;False;Property;_AlphaMultiEndPosition;AlphaMultiEndPosition;24;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;569;496,-960;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;57;-176,-528;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;565;1648,-5456;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;147;1168,-4736;Inherit;False;399;Texture Value;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;87;1984,-1568;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;590;3344,-2800;Inherit;False;396;Distance To Center 01;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;61;752,-576;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;355;1136,-4320;Inherit;False;Property;_C1StartLerp;C1 Start Lerp;28;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;356;1136,-4256;Inherit;False;Property;_C1EndLerp;C1 End Lerp;29;0;Create;True;0;0;0;False;0;False;0;0.835;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;358;1136,-3968;Inherit;False;Property;_C2StartLerp;C2 Start Lerp;31;0;Create;True;0;0;0;False;0;False;0;0.822;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;357;1136,-3888;Inherit;False;Property;_C2EndLerp;C2 End Lerp;32;0;Create;True;0;0;0;False;0;False;0;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;566;1530.328,-4717.531;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;79;1200,-4528;Inherit;False;Property;_Color1;Color1;27;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,0;0.02091414,0.04868957,1.331536,0;True;True;0;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.ColorNode;359;1200,-4176;Inherit;False;Property;_Color2;Color2;30;1;[HDR];Create;True;0;0;0;False;0;False;0.2301886,0.2301886,0.2301886,0;0,2.699597,2.79544,1;True;True;0;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.ColorNode;360;1200,-3792;Inherit;False;Property;_Color3;Color3;33;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0.3471537,1,1;True;True;0;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.SaturateNode;90;2240,-1360;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;186;2128,-1264;Inherit;False;Property;_AlphaMultiPower;AlphaMultiPower;26;0;Create;True;0;0;0;False;0;False;1;30;0.01;30;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;595;3680,-2272;Inherit;False;Property;_MoveCenterDownPower;MoveCenterDownPower;42;0;Create;True;0;0;0;False;0;False;1;0;0.1;20;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;592;3712,-2432;Inherit;False;Property;_MoveCenterDown;MoveCenterDown;40;0;Create;True;0;0;0;False;0;False;0;0;0;100;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;591;3680,-2704;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;152;1184,-640;Inherit;False;Vertical Offset For Waves;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;596;4064,-2384;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;589;1568,-4400;Inherit;False;3 Color Lerp;-1;;12;9ab424eef67864d48b3eb43d05fd6677;0;8;17;FLOAT;0;False;22;COLOR;1,1,1,1;False;18;FLOAT;0;False;19;FLOAT;0;False;23;COLOR;0.5,0.5,0.5,1;False;20;FLOAT;0;False;21;FLOAT;0;False;24;COLOR;0,0,0,1;False;1;COLOR;0
Node;AmplifyShaderEditor.PowerNode;187;2448,-1360;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;451;2304,-1632;Inherit;False;Property;_AlphaMultiStartFactor;AlphaMultiStartFactor;23;0;Create;True;0;0;0;False;0;False;1;0.547;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;452;2288,-1536;Inherit;False;Property;_AlphaMultiEndFactor;AlphaMultiEndFactor;25;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;593;4310.208,-2443.961;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;408;1920,-4256;Inherit;False;ColorRemapped;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;454;2656,-1520;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;153;3856,-2832;Inherit;False;152;Vertical Offset For Waves;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;303;-3040,192;Inherit;False;Scaled Vertices Local;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;594;4240,-2704;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;409;4096,-3392;Inherit;False;408;ColorRemapped;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;204;2848,-1520;Inherit;False;AlphaMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;439;-3696,1072;Inherit;False;1748;466.75;;11;69;71;315;70;321;320;324;322;323;146;440;Adjust Texture Value;0.4609163,0,0.1071862,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;551;-1472,3488;Inherit;False;1956;698.9;;15;541;544;543;545;528;546;529;390;391;530;392;540;538;573;572;Wave Noise;0.1159131,0.03684582,0.1859838,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;446;4144,-2816;Inherit;False;303;Scaled Vertices Local;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;60;4432,-2688;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.BreakToComponentsNode;418;4304,-3248;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.GetLocalVarNode;448;4304,-3056;Inherit;False;204;AlphaMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;601;3936,-2752;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;603;4032,-2544;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;69;-3648,1312;Inherit;False;Property;_RingValueStart;RingValueStart;37;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;71;-3648,1408;Inherit;False;Property;_RingValueEnd;RingValueEnd;39;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;440;-3568,1152;Inherit;False;399;Texture Value;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;315;-3280,1296;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;70;-3216,1136;Inherit;False;Property;_RingValueMid;RingValueMid;38;0;Create;True;0;0;0;False;0;False;0.4645969;0.619;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;321;-2816,1328;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0.5;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;320;-2816,1120;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;324;-2624,1312;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;322;-2624,1216;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;323;-2464,1168;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;146;-2256,1168;Inherit;False;Texture Value Adjusted;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;582;4064,-2976;Inherit;False;Step Antialiasing;-1;;13;2a825e80dfb3290468194f83380797bd;0;2;1;FLOAT;0.01;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;481;-2368,-5136;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;10000;False;1;FLOAT;0
Node;AmplifyShaderEditor.FloorOpNode;483;-2240,-4992;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;482;-2032,-5136;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;484;-1808,-5136;Inherit;False;RandomX2;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;541;-1424,3936;Inherit;False;Property;_WaveNoiseSpeed;WaveNoiseSpeed;16;0;Create;True;0;0;0;False;0;False;0;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;544;-1328,4064;Inherit;False;484;RandomX2;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;543;-1120,3936;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;545;-1088,4048;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;528;-1184,3840;Inherit;False;32;Angle 0 to 1;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;546;-848,4016;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;529;-672,3968;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SamplerNode;390;-480,3936;Inherit;True;Property;_WaveNoiseTex;WaveNoiseTex;14;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.SimpleAddOpNode;391;-144,3968;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;530;-336,3696;Inherit;False;Property;_WaveNoiseStrength;WaveNoiseStrength;15;0;Create;True;0;0;0;False;0;False;0;0;0;4.65;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;392;16,3968;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;540;64,3536;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;519;-4688,-1568;Inherit;True;Property;_TextureSample1;Texture Sample 1;35;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.SamplerNode;520;-4544,-3184;Inherit;True;Property;_TextureSample2;Texture Sample 2;36;0;Create;True;0;0;0;False;0;False;-1;c8e93f5564f8ab24a85cf12fd97fd4a5;c8e93f5564f8ab24a85cf12fd97fd4a5;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.SimpleRemainderNode;553;-2000,-5232;Inherit;False;2;0;INT;0;False;1;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;426;4576,-2816;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StepOpNode;416;4704,-3408;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;417;4592,-3504;Inherit;False;Property;_DebugValue;DebugValue;34;0;Create;True;0;0;0;False;0;False;0;0;0;1.01;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;576;4448,-3360;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SmoothstepOpNode;580;3776,-3248;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;538;-32,3728;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;573;176,3792;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;572;288,3712;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;209;-3280,480;Inherit;False;Scale PT;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;20;-2208,-256;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;27;-1968,-320;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;21;-2208,-368;Inherit;False;1;0;FLOAT;-1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;32;-1712,-288;Inherit;False;Angle 0 to 1;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;583;-64,-368;Inherit;False;sineWavesValue;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;362;4832,-3312;Inherit;False;584;myVarName;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;584;1968,-5408;Inherit;False;myVarName;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;433;4592,-3056;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;579;3760,-3056;Inherit;False;146;Texture Value Adjusted;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectPositionNode;479;-2752,-5392;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;602;3824,-2576;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;600;3456,-2592;Inherit;False;Property;_MoveCenterDownStart;MoveCenterDownStart;41;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;8;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;9;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;10;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;MotionVectors;0;10;MotionVectors;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;False;False;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=MotionVectors;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;-3104,912;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;12;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;5232,-3120;Float;False;True;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;CircularWavePatterns;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;5;False;;10;False;;1;1;False;;10;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;2;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;;0;0;Standard;24;Surface;1;638750341429637981;  Blend;0;0;Two Sided;0;638675602589522618;Forward Only;0;0;Cast Shadows;1;0;  Use Shadow Threshold;0;0;Receive Shadows;1;0;Motion Vectors;1;0;  Add Precomputed Velocity;0;0;GPU Instancing;1;0;LOD CrossFade;1;0;Built-in Fog;1;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Vertex Position,InvertActionOnDeselection;0;638675640535545940;0;11;False;True;True;True;False;False;True;True;True;False;True;False;;False;0
WireConnection;339;0;599;1
WireConnection;337;0;339;0
WireConnection;485;0;599;2
WireConnection;340;0;339;0
WireConnection;340;1;337;0
WireConnection;487;0;485;0
WireConnection;341;0;340;0
WireConnection;486;0;485;0
WireConnection;486;1;487;0
WireConnection;488;0;486;0
WireConnection;526;0;522;0
WireConnection;507;0;500;0
WireConnection;516;0;468;0
WireConnection;524;0;507;0
WireConnection;524;1;526;0
WireConnection;45;0;368;1
WireConnection;45;1;368;3
WireConnection;509;0;524;0
WireConnection;512;0;499;0
WireConnection;527;0;523;0
WireConnection;17;0;45;0
WireConnection;471;0;443;0
WireConnection;477;1;517;0
WireConnection;525;0;527;0
WireConnection;525;1;512;0
WireConnection;18;0;17;0
WireConnection;473;0;471;0
WireConnection;473;1;477;0
WireConnection;363;3;518;0
WireConnection;363;4;470;0
WireConnection;506;0;525;0
WireConnection;506;1;511;0
WireConnection;588;0;586;0
WireConnection;15;0;18;0
WireConnection;15;1;336;2
WireConnection;14;0;18;1
WireConnection;14;1;336;1
WireConnection;193;0;189;0
WireConnection;193;1;191;0
WireConnection;476;0;473;0
WireConnection;505;0;506;0
WireConnection;505;1;363;0
WireConnection;587;0;588;0
WireConnection;13;0;15;0
WireConnection;13;1;14;0
WireConnection;19;0;17;0
WireConnection;19;1;336;0
WireConnection;489;0;599;2
WireConnection;395;0;193;0
WireConnection;495;0;476;0
WireConnection;495;2;510;0
WireConnection;585;0;505;0
WireConnection;585;2;587;0
WireConnection;16;0;13;0
WireConnection;16;1;19;0
WireConnection;491;0;489;0
WireConnection;397;0;395;0
WireConnection;397;1;191;0
WireConnection;445;0;444;0
WireConnection;445;2;585;0
WireConnection;445;3;495;0
WireConnection;161;0;16;0
WireConnection;490;0;489;0
WireConnection;490;1;491;0
WireConnection;396;0;397;0
WireConnection;66;1;445;0
WireConnection;66;3;597;0
WireConnection;66;4;598;0
WireConnection;163;0;84;0
WireConnection;163;1;162;0
WireConnection;163;2;63;0
WireConnection;492;0;490;0
WireConnection;67;0;66;1
WireConnection;67;1;66;2
WireConnection;67;2;66;3
WireConnection;62;0;52;0
WireConnection;62;1;63;0
WireConnection;85;0;163;0
WireConnection;559;0;462;0
WireConnection;559;1;464;0
WireConnection;559;2;465;0
WireConnection;68;0;67;0
WireConnection;547;0;549;0
WireConnection;547;1;550;0
WireConnection;83;0;62;0
WireConnection;83;1;85;0
WireConnection;560;0;559;0
WireConnection;399;0;68;0
WireConnection;577;0;570;0
WireConnection;577;1;578;0
WireConnection;548;0;547;0
WireConnection;548;1;83;0
WireConnection;564;0;560;0
WireConnection;564;1;466;0
WireConnection;569;0;556;0
WireConnection;569;1;56;0
WireConnection;569;2;577;0
WireConnection;57;0;548;0
WireConnection;565;0;562;0
WireConnection;565;1;563;0
WireConnection;565;2;564;0
WireConnection;87;0;228;0
WireConnection;87;1;88;0
WireConnection;87;2;467;0
WireConnection;61;0;569;0
WireConnection;61;1;57;0
WireConnection;566;0;565;0
WireConnection;566;1;147;0
WireConnection;90;0;87;0
WireConnection;591;0;590;0
WireConnection;152;0;61;0
WireConnection;596;0;592;0
WireConnection;596;1;595;0
WireConnection;589;17;566;0
WireConnection;589;22;79;0
WireConnection;589;18;355;0
WireConnection;589;19;356;0
WireConnection;589;23;359;0
WireConnection;589;20;358;0
WireConnection;589;21;357;0
WireConnection;589;24;360;0
WireConnection;187;0;90;0
WireConnection;187;1;186;0
WireConnection;593;0;591;0
WireConnection;593;1;596;0
WireConnection;408;0;589;0
WireConnection;454;0;451;0
WireConnection;454;1;452;0
WireConnection;454;2;187;0
WireConnection;303;0;193;0
WireConnection;594;0;153;0
WireConnection;594;1;593;0
WireConnection;204;0;454;0
WireConnection;60;1;594;0
WireConnection;418;0;409;0
WireConnection;601;0;591;0
WireConnection;601;2;600;0
WireConnection;603;0;601;0
WireConnection;315;0;440;0
WireConnection;315;1;69;0
WireConnection;315;2;71;0
WireConnection;321;0;315;0
WireConnection;321;2;70;0
WireConnection;320;0;315;0
WireConnection;320;2;70;0
WireConnection;324;0;321;0
WireConnection;322;0;320;0
WireConnection;323;0;315;0
WireConnection;323;1;70;0
WireConnection;323;2;322;0
WireConnection;323;3;324;0
WireConnection;146;0;323;0
WireConnection;582;2;579;0
WireConnection;481;0;599;1
WireConnection;483;0;481;0
WireConnection;482;0;481;0
WireConnection;482;1;483;0
WireConnection;484;0;482;0
WireConnection;543;0;541;0
WireConnection;545;0;544;0
WireConnection;546;0;543;0
WireConnection;546;1;545;0
WireConnection;529;0;528;0
WireConnection;529;1;546;0
WireConnection;390;1;529;0
WireConnection;391;0;390;1
WireConnection;391;1;390;2
WireConnection;391;2;390;3
WireConnection;392;0;391;0
WireConnection;540;0;530;0
WireConnection;426;0;446;0
WireConnection;426;1;60;0
WireConnection;416;0;576;0
WireConnection;416;1;417;0
WireConnection;538;0;540;0
WireConnection;538;1;530;0
WireConnection;538;2;392;0
WireConnection;573;1;538;0
WireConnection;209;0;191;0
WireConnection;27;0;16;0
WireConnection;27;1;21;0
WireConnection;27;2;20;0
WireConnection;32;0;27;0
WireConnection;583;0;57;0
WireConnection;584;0;565;0
WireConnection;433;0;418;3
WireConnection;433;1;448;0
WireConnection;602;0;591;0
WireConnection;1;2;409;0
WireConnection;1;3;433;0
WireConnection;1;5;426;0
ASEEND*/
//CHKSM=8D62A2F7854A4CA12C800BBBD6BB205B587C4B4F