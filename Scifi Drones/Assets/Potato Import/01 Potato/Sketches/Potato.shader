// Made with Amplify Shader Editor v1.9.2.1
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Potato"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_Progress("Progress", Range( 0 , 1)) = 0
		_Collection("Collection", Range( 0 , 1)) = 0
		_FusionPoint("FusionPoint", Vector) = (0,0,0,0)
		_CollectionPoint("CollectionPoint", Vector) = (0,0,0,0)
		[HDR]_CenterColor("CenterColor", Color) = (0.2042542,0.5284038,0.9622642,0)
		[HDR]_MainColor("MainColor", Color) = (0.2042542,0.5284038,0.9622642,0)
		_FusionRotationVariance("FusionRotationVariance", Range( 0 , 10)) = 10
		_ParticleScale("ParticleScale", Range( 0 , 0.3)) = 0.3
		_JitterScale("JitterScale", Float) = 0
		_JitterScrollSpeed("JitterScrollSpeed", Float) = 0
		_CollectionStagger("CollectionStagger", Range( 0 , 1)) = 0
		_DelayCatchUp("DelayCatchUp", Range( 0 , 1)) = 1
		_ProgressDelay("ProgressDelay", Range( 0 , 1)) = 1
		_JitterStrength("JitterStrength", Range( 0 , 2)) = 0
		_FusionRotationSpeed("FusionRotationSpeed", Range( 0 , 500)) = 0
		_FusionRotationStrength("FusionRotationStrength", Range( 0 , 1)) = 0
		_MoveToFusionPoint("MoveToFusionPoint", Range( 0 , 1)) = 0
		_OffsetFromFusionPointVariation("OffsetFromFusionPointVariation", Range( 0 , 5)) = 0
		_OffsetFromFusionPointStrength("OffsetFromFusionPointStrength", Range( 0 , 5)) = 0
		_FusionColorRange("FusionColorRange", Float) = 0
		_FusionColorRangeFallOff("FusionColorRangeFallOff", Float) = 0
		_RotateAroundFusionX("RotateAroundFusionX", Float) = 0
		_RotateAroundFusionY("RotateAroundFusionY", Float) = 0
		_RotateAroundFusionZ("RotateAroundFusionZ", Float) = 0
		_UseTime("UseTime", Int) = 0
		_JitterToFusionPointStrength("JitterToFusionPointStrength", Range( 0 , 5)) = 0
		_JitterToFusionPointSpeed("JitterToFusionPointSpeed", Range( 0 , 1)) = 0.3247919
		_JitterToFusionPointScale("JitterToFusionPointScale", Float) = 7.46


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

		[HideInInspector][ToggleOff] _ReceiveShadows("Receive Shadows", Float) = 1.0
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" "UniversalMaterialType"="Unlit" }

		Cull Back
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

			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			

			HLSLPROGRAM

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#pragma multi_compile_instancing
			#pragma instancing_options renderinglayer
			#define shader_feature_local _RECEIVE_SHADOWS_OFF
			#define ASE_SRP_VERSION 140008


			#pragma multi_compile_fragment _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile_fragment _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3

			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON
			#pragma multi_compile _ DYNAMICLIGHTMAP_ON
			#pragma multi_compile_fragment _ DEBUG_DISPLAY
			#pragma multi_compile_fragment _ _WRITE_RENDERING_LAYERS

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_UNLIT

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DBuffer.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Debug/Debugging3D.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Input.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/SurfaceData.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD1;
				#endif
				#ifdef ASE_FOG
					float fogFactor : TEXCOORD2;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _CenterColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _JitterToFusionPointScale;
			float _JitterToFusionPointSpeed;
			float _JitterToFusionPointStrength;
			float _ProgressDelay;
			float _DelayCatchUp;
			float _Progress;
			float _JitterScale;
			float _JitterScrollSpeed;
			float _JitterStrength;
			float _FusionRotationStrength;
			int _UseTime;
			float _FusionRotationVariance;
			float _FusionRotationSpeed;
			float _RotateAroundFusionZ;
			float _RotateAroundFusionY;
			float _RotateAroundFusionX;
			float _OffsetFromFusionPointVariation;
			float _OffsetFromFusionPointStrength;
			float _MoveToFusionPoint;
			float _FusionColorRange;
			float _FusionColorRangeFallOff;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform580 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 MoveToFusionPoinOffset264 = ( ( float4( FusionPoint255 , 0.0 ) - transform215 ) * _MoveToFusionPoint );
				float4 transform491 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform491.z).xx;
				float dotResult4_g113 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g113 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g113 ) * 43758.55 ) ));
				float2 temp_cast_2 = (transform491.x).xx;
				float dotResult4_g111 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g111 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g111 ) * 43758.55 ) ));
				float2 temp_cast_3 = (transform491.y).xx;
				float dotResult4_g112 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g112 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g112 ) * 43758.55 ) ));
				float2 appendResult494 = (float2(lerpResult10_g111 , lerpResult10_g112));
				float2 normalizeResult495 = normalize( appendResult494 );
				float3 appendResult499 = (float3(( _OffsetFromFusionPointStrength * ( 1.0 + ( lerpResult10_g113 * _OffsetFromFusionPointVariation ) ) * normalizeResult495 ) , 0.0));
				float3 OffsetAroundFusionPoint498 = appendResult499;
				float4 transform564 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult548 = (float3(_RotateAroundFusionX , _RotateAroundFusionY , _RotateAroundFusionZ));
				float4 transform284 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_5 = (transform284.x).xx;
				float dotResult4_g114 = dot( temp_cast_5 , float2( 12.9898,78.233 ) );
				float lerpResult10_g114 = lerp( 0.0 , _FusionRotationVariance , frac( ( sin( dotResult4_g114 ) * 43758.55 ) ));
				float temp_output_285_0 = ( _FusionRotationSpeed * ( 1.0 + lerpResult10_g114 ) );
				int UseTime545 = _UseTime;
				float mulTime42 = _TimeParameters.x * (float)UseTime545;
				float4 transform268 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 temp_output_508_0 = ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform268 );
				float3 rotatedValue16 = RotateAroundAxis( FusionPoint255, temp_output_508_0.xyz, normalize( appendResult548 ), radians( ( temp_output_285_0 + ( temp_output_285_0 * mulTime42 ) ) ) );
				float4 FusionRotationOffset250 = ( float4( rotatedValue16 , 0.0 ) - temp_output_508_0 );
				float2 appendResult324 = (float2(-_JitterStrength , _JitterStrength));
				float2 break23_g116 = appendResult324;
				float temp_output_27_0_g116 = _JitterScrollSpeed;
				float2 temp_cast_11 = (temp_output_27_0_g116).xx;
				float4 transform140 = mul(GetWorldToObjectMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_12 = (transform140.x).xx;
				float dotResult4_g104 = dot( temp_cast_12 , float2( 12.9898,78.233 ) );
				float lerpResult10_g104 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g104 ) * 43758.55 ) ));
				float2 temp_cast_13 = (transform140.y).xx;
				float dotResult4_g103 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g103 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g103 ) * 43758.55 ) ));
				float2 temp_cast_14 = (transform140.z).xx;
				float dotResult4_g102 = dot( temp_cast_14 , float2( 12.9898,78.233 ) );
				float lerpResult10_g102 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g102 ) * 43758.55 ) ));
				float4 appendResult171 = (float4(lerpResult10_g104 , lerpResult10_g103 , lerpResult10_g102 , 0.0));
				float3 temp_output_14_0_g116 = appendResult171.xyz;
				float3 break38_g116 = temp_output_14_0_g116;
				float2 appendResult39_g116 = (float2(break38_g116.y , break38_g116.z));
				float2 panner36_g116 = ( 1.0 * _Time.y * temp_cast_11 + appendResult39_g116);
				float temp_output_30_0_g116 = ( 10.0 / _JitterScale );
				float simplePerlin2D37_g116 = snoise( panner36_g116*temp_output_30_0_g116 );
				simplePerlin2D37_g116 = simplePerlin2D37_g116*0.5 + 0.5;
				float lerpResult1_g116 = lerp( break23_g116.x , break23_g116.y , simplePerlin2D37_g116);
				float2 break24_g116 = appendResult324;
				float2 temp_cast_16 = (temp_output_27_0_g116).xx;
				float3 break15_g116 = temp_output_14_0_g116;
				float2 appendResult29_g116 = (float2(break15_g116.x , break15_g116.z));
				float2 panner34_g116 = ( 1.0 * _Time.y * temp_cast_16 + appendResult29_g116);
				float simplePerlin2D6_g116 = snoise( panner34_g116*temp_output_30_0_g116 );
				simplePerlin2D6_g116 = simplePerlin2D6_g116*0.5 + 0.5;
				float lerpResult2_g116 = lerp( break24_g116.x , break24_g116.y , simplePerlin2D6_g116);
				float2 break25_g116 = appendResult324;
				float2 temp_cast_17 = (temp_output_27_0_g116).xx;
				float3 break32_g116 = temp_output_14_0_g116;
				float2 appendResult33_g116 = (float2(break32_g116.x , break32_g116.y));
				float2 panner5_g116 = ( 1.0 * _Time.y * temp_cast_17 + appendResult33_g116);
				float simplePerlin2D35_g116 = snoise( panner5_g116*temp_output_30_0_g116 );
				simplePerlin2D35_g116 = simplePerlin2D35_g116*0.5 + 0.5;
				float lerpResult3_g116 = lerp( break25_g116.x , break25_g116.y , simplePerlin2D35_g116);
				float3 appendResult10_g116 = (float3(lerpResult1_g116 , lerpResult2_g116 , lerpResult3_g116));
				float3 JitterOffset261 = appendResult10_g116;
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_19 = (transform333.x).xx;
				float dotResult4_g115 = dot( temp_cast_19 , float2( 12.9898,78.233 ) );
				float lerpResult10_g115 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g115 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g115;
				float temp_output_436_0 = ( _DelayCatchUp + _ProgressDelay );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay );
				float temp_output_567_0 = ( RandomViaX566 * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUp;
				float DelayEffect175 = saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) );
				float4 lerpResult579 = lerp( transform580 , ( ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform564 ) + ( float4( 0,0,0,0 ) + ( FusionRotationOffset250 * _FusionRotationStrength ) ) + float4( JitterOffset261 , 0.0 ) ) , DelayEffect175);
				float4 FinalPosition536 = lerpResult579;
				float2 temp_cast_21 = (( ( _JitterToFusionPointSpeed * _TimeParameters.x ) + RandomViaX566 )).xx;
				float simplePerlin2D569 = snoise( temp_cast_21*_JitterToFusionPointScale );
				simplePerlin2D569 = simplePerlin2D569*0.5 + 0.5;
				float4 temp_output_561_0 = ( lerpResult579 + ( ( float4( FusionPoint255 , 0.0 ) - FinalPosition536 ) * ( _JitterToFusionPointStrength * saturate( ( simplePerlin2D569 - 0.1 ) ) ) ) );
				float temp_output_8_0_g29 = ( _CollectionStagger * 0.5 );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_24 = (transform267.x).xx;
				float dotResult4_g30 = dot( temp_cast_24 , float2( 12.9898,78.233 ) );
				float lerpResult10_g30 = lerp( 0.0 , temp_output_8_0_g29 , frac( ( sin( dotResult4_g30 ) * 43758.55 ) ));
				float temp_output_4_0_g29 = lerpResult10_g30;
				float clampResult6_g29 = clamp( ( _Collection - temp_output_4_0_g29 ) , 0.0 , 1.0 );
				float lerpResult23_g29 = lerp( ( 1.0 - temp_output_8_0_g29 ) , ( 1.0 - temp_output_4_0_g29 ) , 0.0);
				float clampResult19_g29 = clamp( (0.0 + (clampResult6_g29 - 0.0) * (1.0 - 0.0) / (lerpResult23_g29 - 0.0)) , 0.0 , 1.0 );
				float CollectionAdjusted219 = clampResult19_g29;
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( temp_output_561_0 + ( ( float4( _CollectionPoint , 0.0 ) - temp_output_561_0 ) * CollectionAdjusted219 ) ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj26;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				#ifdef ASE_FOG
					o.fogFactor = ComputeFogFactor( positionCS.z );
				#endif

				o.clipPos = positionCS;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
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
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag ( VertexOutput IN
				#ifdef _WRITE_RENDERING_LAYERS
				, out float4 outRenderingLayers : SV_Target1
				#endif
				 ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 WorldPosition = IN.worldPos;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float3 FusionPoint255 = _FusionPoint;
				float4 transform580 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 MoveToFusionPoinOffset264 = ( ( float4( FusionPoint255 , 0.0 ) - transform215 ) * _MoveToFusionPoint );
				float4 transform491 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_2 = (transform491.z).xx;
				float dotResult4_g113 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g113 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g113 ) * 43758.55 ) ));
				float2 temp_cast_3 = (transform491.x).xx;
				float dotResult4_g111 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g111 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g111 ) * 43758.55 ) ));
				float2 temp_cast_4 = (transform491.y).xx;
				float dotResult4_g112 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g112 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g112 ) * 43758.55 ) ));
				float2 appendResult494 = (float2(lerpResult10_g111 , lerpResult10_g112));
				float2 normalizeResult495 = normalize( appendResult494 );
				float3 appendResult499 = (float3(( _OffsetFromFusionPointStrength * ( 1.0 + ( lerpResult10_g113 * _OffsetFromFusionPointVariation ) ) * normalizeResult495 ) , 0.0));
				float3 OffsetAroundFusionPoint498 = appendResult499;
				float4 transform564 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult548 = (float3(_RotateAroundFusionX , _RotateAroundFusionY , _RotateAroundFusionZ));
				float4 transform284 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_6 = (transform284.x).xx;
				float dotResult4_g114 = dot( temp_cast_6 , float2( 12.9898,78.233 ) );
				float lerpResult10_g114 = lerp( 0.0 , _FusionRotationVariance , frac( ( sin( dotResult4_g114 ) * 43758.55 ) ));
				float temp_output_285_0 = ( _FusionRotationSpeed * ( 1.0 + lerpResult10_g114 ) );
				int UseTime545 = _UseTime;
				float mulTime42 = _TimeParameters.x * (float)UseTime545;
				float4 transform268 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 temp_output_508_0 = ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform268 );
				float3 rotatedValue16 = RotateAroundAxis( FusionPoint255, temp_output_508_0.xyz, normalize( appendResult548 ), radians( ( temp_output_285_0 + ( temp_output_285_0 * mulTime42 ) ) ) );
				float4 FusionRotationOffset250 = ( float4( rotatedValue16 , 0.0 ) - temp_output_508_0 );
				float2 appendResult324 = (float2(-_JitterStrength , _JitterStrength));
				float2 break23_g116 = appendResult324;
				float temp_output_27_0_g116 = _JitterScrollSpeed;
				float2 temp_cast_12 = (temp_output_27_0_g116).xx;
				float4 transform140 = mul(GetWorldToObjectMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_13 = (transform140.x).xx;
				float dotResult4_g104 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g104 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g104 ) * 43758.55 ) ));
				float2 temp_cast_14 = (transform140.y).xx;
				float dotResult4_g103 = dot( temp_cast_14 , float2( 12.9898,78.233 ) );
				float lerpResult10_g103 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g103 ) * 43758.55 ) ));
				float2 temp_cast_15 = (transform140.z).xx;
				float dotResult4_g102 = dot( temp_cast_15 , float2( 12.9898,78.233 ) );
				float lerpResult10_g102 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g102 ) * 43758.55 ) ));
				float4 appendResult171 = (float4(lerpResult10_g104 , lerpResult10_g103 , lerpResult10_g102 , 0.0));
				float3 temp_output_14_0_g116 = appendResult171.xyz;
				float3 break38_g116 = temp_output_14_0_g116;
				float2 appendResult39_g116 = (float2(break38_g116.y , break38_g116.z));
				float2 panner36_g116 = ( 1.0 * _Time.y * temp_cast_12 + appendResult39_g116);
				float temp_output_30_0_g116 = ( 10.0 / _JitterScale );
				float simplePerlin2D37_g116 = snoise( panner36_g116*temp_output_30_0_g116 );
				simplePerlin2D37_g116 = simplePerlin2D37_g116*0.5 + 0.5;
				float lerpResult1_g116 = lerp( break23_g116.x , break23_g116.y , simplePerlin2D37_g116);
				float2 break24_g116 = appendResult324;
				float2 temp_cast_17 = (temp_output_27_0_g116).xx;
				float3 break15_g116 = temp_output_14_0_g116;
				float2 appendResult29_g116 = (float2(break15_g116.x , break15_g116.z));
				float2 panner34_g116 = ( 1.0 * _Time.y * temp_cast_17 + appendResult29_g116);
				float simplePerlin2D6_g116 = snoise( panner34_g116*temp_output_30_0_g116 );
				simplePerlin2D6_g116 = simplePerlin2D6_g116*0.5 + 0.5;
				float lerpResult2_g116 = lerp( break24_g116.x , break24_g116.y , simplePerlin2D6_g116);
				float2 break25_g116 = appendResult324;
				float2 temp_cast_18 = (temp_output_27_0_g116).xx;
				float3 break32_g116 = temp_output_14_0_g116;
				float2 appendResult33_g116 = (float2(break32_g116.x , break32_g116.y));
				float2 panner5_g116 = ( 1.0 * _Time.y * temp_cast_18 + appendResult33_g116);
				float simplePerlin2D35_g116 = snoise( panner5_g116*temp_output_30_0_g116 );
				simplePerlin2D35_g116 = simplePerlin2D35_g116*0.5 + 0.5;
				float lerpResult3_g116 = lerp( break25_g116.x , break25_g116.y , simplePerlin2D35_g116);
				float3 appendResult10_g116 = (float3(lerpResult1_g116 , lerpResult2_g116 , lerpResult3_g116));
				float3 JitterOffset261 = appendResult10_g116;
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_20 = (transform333.x).xx;
				float dotResult4_g115 = dot( temp_cast_20 , float2( 12.9898,78.233 ) );
				float lerpResult10_g115 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g115 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g115;
				float temp_output_436_0 = ( _DelayCatchUp + _ProgressDelay );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay );
				float temp_output_567_0 = ( RandomViaX566 * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUp;
				float DelayEffect175 = saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) );
				float4 lerpResult579 = lerp( transform580 , ( ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform564 ) + ( float4( 0,0,0,0 ) + ( FusionRotationOffset250 * _FusionRotationStrength ) ) + float4( JitterOffset261 , 0.0 ) ) , DelayEffect175);
				float4 FinalPosition536 = lerpResult579;
				float temp_output_516_0 = distance( float4( FusionPoint255 , 0.0 ) , FinalPosition536 );
				float FusionColorAmount526 = saturate( ( ( temp_output_516_0 <= _FusionColorRange ? 1.0 : (1.0 + (( temp_output_516_0 - _FusionColorRange ) - 0.0) * (0.0 - 1.0) / (_FusionColorRangeFallOff - 0.0)) ) * ( temp_output_516_0 > ( _FusionColorRange + _FusionColorRangeFallOff ) ? 0.0 : 1.0 ) ) );
				float4 lerpResult523 = lerp( _MainColor , _CenterColor , FusionColorAmount526);
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = lerpResult523.rgb;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					clip( Alpha - AlphaClipThreshold );
				#endif

				#if defined(_DBUFFER)
					ApplyDecalToBaseColor(IN.clipPos, Color);
				#endif

				#if defined(_ALPHAPREMULTIPLY_ON)
				Color *= Alpha;
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.clipPos );
				#endif

				#ifdef ASE_FOG
					Color = MixFog( Color, IN.fogFactor );
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
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#pragma multi_compile_instancing
			#define shader_feature_local _RECEIVE_SHADOWS_OFF
			#define ASE_SRP_VERSION 140008


			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _CenterColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _JitterToFusionPointScale;
			float _JitterToFusionPointSpeed;
			float _JitterToFusionPointStrength;
			float _ProgressDelay;
			float _DelayCatchUp;
			float _Progress;
			float _JitterScale;
			float _JitterScrollSpeed;
			float _JitterStrength;
			float _FusionRotationStrength;
			int _UseTime;
			float _FusionRotationVariance;
			float _FusionRotationSpeed;
			float _RotateAroundFusionZ;
			float _RotateAroundFusionY;
			float _RotateAroundFusionX;
			float _OffsetFromFusionPointVariation;
			float _OffsetFromFusionPointStrength;
			float _MoveToFusionPoint;
			float _FusionColorRange;
			float _FusionColorRangeFallOff;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform580 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 MoveToFusionPoinOffset264 = ( ( float4( FusionPoint255 , 0.0 ) - transform215 ) * _MoveToFusionPoint );
				float4 transform491 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform491.z).xx;
				float dotResult4_g113 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g113 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g113 ) * 43758.55 ) ));
				float2 temp_cast_2 = (transform491.x).xx;
				float dotResult4_g111 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g111 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g111 ) * 43758.55 ) ));
				float2 temp_cast_3 = (transform491.y).xx;
				float dotResult4_g112 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g112 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g112 ) * 43758.55 ) ));
				float2 appendResult494 = (float2(lerpResult10_g111 , lerpResult10_g112));
				float2 normalizeResult495 = normalize( appendResult494 );
				float3 appendResult499 = (float3(( _OffsetFromFusionPointStrength * ( 1.0 + ( lerpResult10_g113 * _OffsetFromFusionPointVariation ) ) * normalizeResult495 ) , 0.0));
				float3 OffsetAroundFusionPoint498 = appendResult499;
				float4 transform564 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult548 = (float3(_RotateAroundFusionX , _RotateAroundFusionY , _RotateAroundFusionZ));
				float4 transform284 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_5 = (transform284.x).xx;
				float dotResult4_g114 = dot( temp_cast_5 , float2( 12.9898,78.233 ) );
				float lerpResult10_g114 = lerp( 0.0 , _FusionRotationVariance , frac( ( sin( dotResult4_g114 ) * 43758.55 ) ));
				float temp_output_285_0 = ( _FusionRotationSpeed * ( 1.0 + lerpResult10_g114 ) );
				int UseTime545 = _UseTime;
				float mulTime42 = _TimeParameters.x * (float)UseTime545;
				float4 transform268 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 temp_output_508_0 = ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform268 );
				float3 rotatedValue16 = RotateAroundAxis( FusionPoint255, temp_output_508_0.xyz, normalize( appendResult548 ), radians( ( temp_output_285_0 + ( temp_output_285_0 * mulTime42 ) ) ) );
				float4 FusionRotationOffset250 = ( float4( rotatedValue16 , 0.0 ) - temp_output_508_0 );
				float2 appendResult324 = (float2(-_JitterStrength , _JitterStrength));
				float2 break23_g116 = appendResult324;
				float temp_output_27_0_g116 = _JitterScrollSpeed;
				float2 temp_cast_11 = (temp_output_27_0_g116).xx;
				float4 transform140 = mul(GetWorldToObjectMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_12 = (transform140.x).xx;
				float dotResult4_g104 = dot( temp_cast_12 , float2( 12.9898,78.233 ) );
				float lerpResult10_g104 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g104 ) * 43758.55 ) ));
				float2 temp_cast_13 = (transform140.y).xx;
				float dotResult4_g103 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g103 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g103 ) * 43758.55 ) ));
				float2 temp_cast_14 = (transform140.z).xx;
				float dotResult4_g102 = dot( temp_cast_14 , float2( 12.9898,78.233 ) );
				float lerpResult10_g102 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g102 ) * 43758.55 ) ));
				float4 appendResult171 = (float4(lerpResult10_g104 , lerpResult10_g103 , lerpResult10_g102 , 0.0));
				float3 temp_output_14_0_g116 = appendResult171.xyz;
				float3 break38_g116 = temp_output_14_0_g116;
				float2 appendResult39_g116 = (float2(break38_g116.y , break38_g116.z));
				float2 panner36_g116 = ( 1.0 * _Time.y * temp_cast_11 + appendResult39_g116);
				float temp_output_30_0_g116 = ( 10.0 / _JitterScale );
				float simplePerlin2D37_g116 = snoise( panner36_g116*temp_output_30_0_g116 );
				simplePerlin2D37_g116 = simplePerlin2D37_g116*0.5 + 0.5;
				float lerpResult1_g116 = lerp( break23_g116.x , break23_g116.y , simplePerlin2D37_g116);
				float2 break24_g116 = appendResult324;
				float2 temp_cast_16 = (temp_output_27_0_g116).xx;
				float3 break15_g116 = temp_output_14_0_g116;
				float2 appendResult29_g116 = (float2(break15_g116.x , break15_g116.z));
				float2 panner34_g116 = ( 1.0 * _Time.y * temp_cast_16 + appendResult29_g116);
				float simplePerlin2D6_g116 = snoise( panner34_g116*temp_output_30_0_g116 );
				simplePerlin2D6_g116 = simplePerlin2D6_g116*0.5 + 0.5;
				float lerpResult2_g116 = lerp( break24_g116.x , break24_g116.y , simplePerlin2D6_g116);
				float2 break25_g116 = appendResult324;
				float2 temp_cast_17 = (temp_output_27_0_g116).xx;
				float3 break32_g116 = temp_output_14_0_g116;
				float2 appendResult33_g116 = (float2(break32_g116.x , break32_g116.y));
				float2 panner5_g116 = ( 1.0 * _Time.y * temp_cast_17 + appendResult33_g116);
				float simplePerlin2D35_g116 = snoise( panner5_g116*temp_output_30_0_g116 );
				simplePerlin2D35_g116 = simplePerlin2D35_g116*0.5 + 0.5;
				float lerpResult3_g116 = lerp( break25_g116.x , break25_g116.y , simplePerlin2D35_g116);
				float3 appendResult10_g116 = (float3(lerpResult1_g116 , lerpResult2_g116 , lerpResult3_g116));
				float3 JitterOffset261 = appendResult10_g116;
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_19 = (transform333.x).xx;
				float dotResult4_g115 = dot( temp_cast_19 , float2( 12.9898,78.233 ) );
				float lerpResult10_g115 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g115 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g115;
				float temp_output_436_0 = ( _DelayCatchUp + _ProgressDelay );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay );
				float temp_output_567_0 = ( RandomViaX566 * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUp;
				float DelayEffect175 = saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) );
				float4 lerpResult579 = lerp( transform580 , ( ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform564 ) + ( float4( 0,0,0,0 ) + ( FusionRotationOffset250 * _FusionRotationStrength ) ) + float4( JitterOffset261 , 0.0 ) ) , DelayEffect175);
				float4 FinalPosition536 = lerpResult579;
				float2 temp_cast_21 = (( ( _JitterToFusionPointSpeed * _TimeParameters.x ) + RandomViaX566 )).xx;
				float simplePerlin2D569 = snoise( temp_cast_21*_JitterToFusionPointScale );
				simplePerlin2D569 = simplePerlin2D569*0.5 + 0.5;
				float4 temp_output_561_0 = ( lerpResult579 + ( ( float4( FusionPoint255 , 0.0 ) - FinalPosition536 ) * ( _JitterToFusionPointStrength * saturate( ( simplePerlin2D569 - 0.1 ) ) ) ) );
				float temp_output_8_0_g29 = ( _CollectionStagger * 0.5 );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_24 = (transform267.x).xx;
				float dotResult4_g30 = dot( temp_cast_24 , float2( 12.9898,78.233 ) );
				float lerpResult10_g30 = lerp( 0.0 , temp_output_8_0_g29 , frac( ( sin( dotResult4_g30 ) * 43758.55 ) ));
				float temp_output_4_0_g29 = lerpResult10_g30;
				float clampResult6_g29 = clamp( ( _Collection - temp_output_4_0_g29 ) , 0.0 , 1.0 );
				float lerpResult23_g29 = lerp( ( 1.0 - temp_output_8_0_g29 ) , ( 1.0 - temp_output_4_0_g29 ) , 0.0);
				float clampResult19_g29 = clamp( (0.0 + (clampResult6_g29 - 0.0) * (1.0 - 0.0) / (lerpResult23_g29 - 0.0)) , 0.0 , 1.0 );
				float CollectionAdjusted219 = clampResult19_g29;
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( temp_output_561_0 + ( ( float4( _CollectionPoint , 0.0 ) - temp_output_561_0 ) * CollectionAdjusted219 ) ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj26;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					o.worldPos = positionWS;
				#endif

				o.clipPos = TransformWorldToHClip( positionWS );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
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
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				

				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.clipPos );
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

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define shader_feature_local _RECEIVE_SHADOWS_OFF
			#define ASE_SRP_VERSION 140008


			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _CenterColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _JitterToFusionPointScale;
			float _JitterToFusionPointSpeed;
			float _JitterToFusionPointStrength;
			float _ProgressDelay;
			float _DelayCatchUp;
			float _Progress;
			float _JitterScale;
			float _JitterScrollSpeed;
			float _JitterStrength;
			float _FusionRotationStrength;
			int _UseTime;
			float _FusionRotationVariance;
			float _FusionRotationSpeed;
			float _RotateAroundFusionZ;
			float _RotateAroundFusionY;
			float _RotateAroundFusionX;
			float _OffsetFromFusionPointVariation;
			float _OffsetFromFusionPointStrength;
			float _MoveToFusionPoint;
			float _FusionColorRange;
			float _FusionColorRangeFallOff;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			int _ObjectId;
			int _PassValue;

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction(VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform580 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 MoveToFusionPoinOffset264 = ( ( float4( FusionPoint255 , 0.0 ) - transform215 ) * _MoveToFusionPoint );
				float4 transform491 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform491.z).xx;
				float dotResult4_g113 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g113 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g113 ) * 43758.55 ) ));
				float2 temp_cast_2 = (transform491.x).xx;
				float dotResult4_g111 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g111 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g111 ) * 43758.55 ) ));
				float2 temp_cast_3 = (transform491.y).xx;
				float dotResult4_g112 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g112 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g112 ) * 43758.55 ) ));
				float2 appendResult494 = (float2(lerpResult10_g111 , lerpResult10_g112));
				float2 normalizeResult495 = normalize( appendResult494 );
				float3 appendResult499 = (float3(( _OffsetFromFusionPointStrength * ( 1.0 + ( lerpResult10_g113 * _OffsetFromFusionPointVariation ) ) * normalizeResult495 ) , 0.0));
				float3 OffsetAroundFusionPoint498 = appendResult499;
				float4 transform564 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult548 = (float3(_RotateAroundFusionX , _RotateAroundFusionY , _RotateAroundFusionZ));
				float4 transform284 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_5 = (transform284.x).xx;
				float dotResult4_g114 = dot( temp_cast_5 , float2( 12.9898,78.233 ) );
				float lerpResult10_g114 = lerp( 0.0 , _FusionRotationVariance , frac( ( sin( dotResult4_g114 ) * 43758.55 ) ));
				float temp_output_285_0 = ( _FusionRotationSpeed * ( 1.0 + lerpResult10_g114 ) );
				int UseTime545 = _UseTime;
				float mulTime42 = _TimeParameters.x * (float)UseTime545;
				float4 transform268 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 temp_output_508_0 = ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform268 );
				float3 rotatedValue16 = RotateAroundAxis( FusionPoint255, temp_output_508_0.xyz, normalize( appendResult548 ), radians( ( temp_output_285_0 + ( temp_output_285_0 * mulTime42 ) ) ) );
				float4 FusionRotationOffset250 = ( float4( rotatedValue16 , 0.0 ) - temp_output_508_0 );
				float2 appendResult324 = (float2(-_JitterStrength , _JitterStrength));
				float2 break23_g116 = appendResult324;
				float temp_output_27_0_g116 = _JitterScrollSpeed;
				float2 temp_cast_11 = (temp_output_27_0_g116).xx;
				float4 transform140 = mul(GetWorldToObjectMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_12 = (transform140.x).xx;
				float dotResult4_g104 = dot( temp_cast_12 , float2( 12.9898,78.233 ) );
				float lerpResult10_g104 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g104 ) * 43758.55 ) ));
				float2 temp_cast_13 = (transform140.y).xx;
				float dotResult4_g103 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g103 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g103 ) * 43758.55 ) ));
				float2 temp_cast_14 = (transform140.z).xx;
				float dotResult4_g102 = dot( temp_cast_14 , float2( 12.9898,78.233 ) );
				float lerpResult10_g102 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g102 ) * 43758.55 ) ));
				float4 appendResult171 = (float4(lerpResult10_g104 , lerpResult10_g103 , lerpResult10_g102 , 0.0));
				float3 temp_output_14_0_g116 = appendResult171.xyz;
				float3 break38_g116 = temp_output_14_0_g116;
				float2 appendResult39_g116 = (float2(break38_g116.y , break38_g116.z));
				float2 panner36_g116 = ( 1.0 * _Time.y * temp_cast_11 + appendResult39_g116);
				float temp_output_30_0_g116 = ( 10.0 / _JitterScale );
				float simplePerlin2D37_g116 = snoise( panner36_g116*temp_output_30_0_g116 );
				simplePerlin2D37_g116 = simplePerlin2D37_g116*0.5 + 0.5;
				float lerpResult1_g116 = lerp( break23_g116.x , break23_g116.y , simplePerlin2D37_g116);
				float2 break24_g116 = appendResult324;
				float2 temp_cast_16 = (temp_output_27_0_g116).xx;
				float3 break15_g116 = temp_output_14_0_g116;
				float2 appendResult29_g116 = (float2(break15_g116.x , break15_g116.z));
				float2 panner34_g116 = ( 1.0 * _Time.y * temp_cast_16 + appendResult29_g116);
				float simplePerlin2D6_g116 = snoise( panner34_g116*temp_output_30_0_g116 );
				simplePerlin2D6_g116 = simplePerlin2D6_g116*0.5 + 0.5;
				float lerpResult2_g116 = lerp( break24_g116.x , break24_g116.y , simplePerlin2D6_g116);
				float2 break25_g116 = appendResult324;
				float2 temp_cast_17 = (temp_output_27_0_g116).xx;
				float3 break32_g116 = temp_output_14_0_g116;
				float2 appendResult33_g116 = (float2(break32_g116.x , break32_g116.y));
				float2 panner5_g116 = ( 1.0 * _Time.y * temp_cast_17 + appendResult33_g116);
				float simplePerlin2D35_g116 = snoise( panner5_g116*temp_output_30_0_g116 );
				simplePerlin2D35_g116 = simplePerlin2D35_g116*0.5 + 0.5;
				float lerpResult3_g116 = lerp( break25_g116.x , break25_g116.y , simplePerlin2D35_g116);
				float3 appendResult10_g116 = (float3(lerpResult1_g116 , lerpResult2_g116 , lerpResult3_g116));
				float3 JitterOffset261 = appendResult10_g116;
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_19 = (transform333.x).xx;
				float dotResult4_g115 = dot( temp_cast_19 , float2( 12.9898,78.233 ) );
				float lerpResult10_g115 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g115 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g115;
				float temp_output_436_0 = ( _DelayCatchUp + _ProgressDelay );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay );
				float temp_output_567_0 = ( RandomViaX566 * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUp;
				float DelayEffect175 = saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) );
				float4 lerpResult579 = lerp( transform580 , ( ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform564 ) + ( float4( 0,0,0,0 ) + ( FusionRotationOffset250 * _FusionRotationStrength ) ) + float4( JitterOffset261 , 0.0 ) ) , DelayEffect175);
				float4 FinalPosition536 = lerpResult579;
				float2 temp_cast_21 = (( ( _JitterToFusionPointSpeed * _TimeParameters.x ) + RandomViaX566 )).xx;
				float simplePerlin2D569 = snoise( temp_cast_21*_JitterToFusionPointScale );
				simplePerlin2D569 = simplePerlin2D569*0.5 + 0.5;
				float4 temp_output_561_0 = ( lerpResult579 + ( ( float4( FusionPoint255 , 0.0 ) - FinalPosition536 ) * ( _JitterToFusionPointStrength * saturate( ( simplePerlin2D569 - 0.1 ) ) ) ) );
				float temp_output_8_0_g29 = ( _CollectionStagger * 0.5 );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_24 = (transform267.x).xx;
				float dotResult4_g30 = dot( temp_cast_24 , float2( 12.9898,78.233 ) );
				float lerpResult10_g30 = lerp( 0.0 , temp_output_8_0_g29 , frac( ( sin( dotResult4_g30 ) * 43758.55 ) ));
				float temp_output_4_0_g29 = lerpResult10_g30;
				float clampResult6_g29 = clamp( ( _Collection - temp_output_4_0_g29 ) , 0.0 , 1.0 );
				float lerpResult23_g29 = lerp( ( 1.0 - temp_output_8_0_g29 ) , ( 1.0 - temp_output_4_0_g29 ) , 0.0);
				float clampResult19_g29 = clamp( (0.0 + (clampResult6_g29 - 0.0) * (1.0 - 0.0) / (lerpResult23_g29 - 0.0)) , 0.0 , 1.0 );
				float CollectionAdjusted219 = clampResult19_g29;
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( temp_output_561_0 + ( ( float4( _CollectionPoint , 0.0 ) - temp_output_561_0 ) * CollectionAdjusted219 ) ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj26;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				o.clipPos = TransformWorldToHClip(positionWS);

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
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
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				

				surfaceDescription.Alpha = 1;
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

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define shader_feature_local _RECEIVE_SHADOWS_OFF
			#define ASE_SRP_VERSION 140008


			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _CenterColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _JitterToFusionPointScale;
			float _JitterToFusionPointSpeed;
			float _JitterToFusionPointStrength;
			float _ProgressDelay;
			float _DelayCatchUp;
			float _Progress;
			float _JitterScale;
			float _JitterScrollSpeed;
			float _JitterStrength;
			float _FusionRotationStrength;
			int _UseTime;
			float _FusionRotationVariance;
			float _FusionRotationSpeed;
			float _RotateAroundFusionZ;
			float _RotateAroundFusionY;
			float _RotateAroundFusionX;
			float _OffsetFromFusionPointVariation;
			float _OffsetFromFusionPointStrength;
			float _MoveToFusionPoint;
			float _FusionColorRange;
			float _FusionColorRangeFallOff;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			float4 _SelectionID;

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction(VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform580 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 MoveToFusionPoinOffset264 = ( ( float4( FusionPoint255 , 0.0 ) - transform215 ) * _MoveToFusionPoint );
				float4 transform491 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform491.z).xx;
				float dotResult4_g113 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g113 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g113 ) * 43758.55 ) ));
				float2 temp_cast_2 = (transform491.x).xx;
				float dotResult4_g111 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g111 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g111 ) * 43758.55 ) ));
				float2 temp_cast_3 = (transform491.y).xx;
				float dotResult4_g112 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g112 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g112 ) * 43758.55 ) ));
				float2 appendResult494 = (float2(lerpResult10_g111 , lerpResult10_g112));
				float2 normalizeResult495 = normalize( appendResult494 );
				float3 appendResult499 = (float3(( _OffsetFromFusionPointStrength * ( 1.0 + ( lerpResult10_g113 * _OffsetFromFusionPointVariation ) ) * normalizeResult495 ) , 0.0));
				float3 OffsetAroundFusionPoint498 = appendResult499;
				float4 transform564 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult548 = (float3(_RotateAroundFusionX , _RotateAroundFusionY , _RotateAroundFusionZ));
				float4 transform284 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_5 = (transform284.x).xx;
				float dotResult4_g114 = dot( temp_cast_5 , float2( 12.9898,78.233 ) );
				float lerpResult10_g114 = lerp( 0.0 , _FusionRotationVariance , frac( ( sin( dotResult4_g114 ) * 43758.55 ) ));
				float temp_output_285_0 = ( _FusionRotationSpeed * ( 1.0 + lerpResult10_g114 ) );
				int UseTime545 = _UseTime;
				float mulTime42 = _TimeParameters.x * (float)UseTime545;
				float4 transform268 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 temp_output_508_0 = ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform268 );
				float3 rotatedValue16 = RotateAroundAxis( FusionPoint255, temp_output_508_0.xyz, normalize( appendResult548 ), radians( ( temp_output_285_0 + ( temp_output_285_0 * mulTime42 ) ) ) );
				float4 FusionRotationOffset250 = ( float4( rotatedValue16 , 0.0 ) - temp_output_508_0 );
				float2 appendResult324 = (float2(-_JitterStrength , _JitterStrength));
				float2 break23_g116 = appendResult324;
				float temp_output_27_0_g116 = _JitterScrollSpeed;
				float2 temp_cast_11 = (temp_output_27_0_g116).xx;
				float4 transform140 = mul(GetWorldToObjectMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_12 = (transform140.x).xx;
				float dotResult4_g104 = dot( temp_cast_12 , float2( 12.9898,78.233 ) );
				float lerpResult10_g104 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g104 ) * 43758.55 ) ));
				float2 temp_cast_13 = (transform140.y).xx;
				float dotResult4_g103 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g103 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g103 ) * 43758.55 ) ));
				float2 temp_cast_14 = (transform140.z).xx;
				float dotResult4_g102 = dot( temp_cast_14 , float2( 12.9898,78.233 ) );
				float lerpResult10_g102 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g102 ) * 43758.55 ) ));
				float4 appendResult171 = (float4(lerpResult10_g104 , lerpResult10_g103 , lerpResult10_g102 , 0.0));
				float3 temp_output_14_0_g116 = appendResult171.xyz;
				float3 break38_g116 = temp_output_14_0_g116;
				float2 appendResult39_g116 = (float2(break38_g116.y , break38_g116.z));
				float2 panner36_g116 = ( 1.0 * _Time.y * temp_cast_11 + appendResult39_g116);
				float temp_output_30_0_g116 = ( 10.0 / _JitterScale );
				float simplePerlin2D37_g116 = snoise( panner36_g116*temp_output_30_0_g116 );
				simplePerlin2D37_g116 = simplePerlin2D37_g116*0.5 + 0.5;
				float lerpResult1_g116 = lerp( break23_g116.x , break23_g116.y , simplePerlin2D37_g116);
				float2 break24_g116 = appendResult324;
				float2 temp_cast_16 = (temp_output_27_0_g116).xx;
				float3 break15_g116 = temp_output_14_0_g116;
				float2 appendResult29_g116 = (float2(break15_g116.x , break15_g116.z));
				float2 panner34_g116 = ( 1.0 * _Time.y * temp_cast_16 + appendResult29_g116);
				float simplePerlin2D6_g116 = snoise( panner34_g116*temp_output_30_0_g116 );
				simplePerlin2D6_g116 = simplePerlin2D6_g116*0.5 + 0.5;
				float lerpResult2_g116 = lerp( break24_g116.x , break24_g116.y , simplePerlin2D6_g116);
				float2 break25_g116 = appendResult324;
				float2 temp_cast_17 = (temp_output_27_0_g116).xx;
				float3 break32_g116 = temp_output_14_0_g116;
				float2 appendResult33_g116 = (float2(break32_g116.x , break32_g116.y));
				float2 panner5_g116 = ( 1.0 * _Time.y * temp_cast_17 + appendResult33_g116);
				float simplePerlin2D35_g116 = snoise( panner5_g116*temp_output_30_0_g116 );
				simplePerlin2D35_g116 = simplePerlin2D35_g116*0.5 + 0.5;
				float lerpResult3_g116 = lerp( break25_g116.x , break25_g116.y , simplePerlin2D35_g116);
				float3 appendResult10_g116 = (float3(lerpResult1_g116 , lerpResult2_g116 , lerpResult3_g116));
				float3 JitterOffset261 = appendResult10_g116;
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_19 = (transform333.x).xx;
				float dotResult4_g115 = dot( temp_cast_19 , float2( 12.9898,78.233 ) );
				float lerpResult10_g115 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g115 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g115;
				float temp_output_436_0 = ( _DelayCatchUp + _ProgressDelay );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay );
				float temp_output_567_0 = ( RandomViaX566 * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUp;
				float DelayEffect175 = saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) );
				float4 lerpResult579 = lerp( transform580 , ( ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform564 ) + ( float4( 0,0,0,0 ) + ( FusionRotationOffset250 * _FusionRotationStrength ) ) + float4( JitterOffset261 , 0.0 ) ) , DelayEffect175);
				float4 FinalPosition536 = lerpResult579;
				float2 temp_cast_21 = (( ( _JitterToFusionPointSpeed * _TimeParameters.x ) + RandomViaX566 )).xx;
				float simplePerlin2D569 = snoise( temp_cast_21*_JitterToFusionPointScale );
				simplePerlin2D569 = simplePerlin2D569*0.5 + 0.5;
				float4 temp_output_561_0 = ( lerpResult579 + ( ( float4( FusionPoint255 , 0.0 ) - FinalPosition536 ) * ( _JitterToFusionPointStrength * saturate( ( simplePerlin2D569 - 0.1 ) ) ) ) );
				float temp_output_8_0_g29 = ( _CollectionStagger * 0.5 );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_24 = (transform267.x).xx;
				float dotResult4_g30 = dot( temp_cast_24 , float2( 12.9898,78.233 ) );
				float lerpResult10_g30 = lerp( 0.0 , temp_output_8_0_g29 , frac( ( sin( dotResult4_g30 ) * 43758.55 ) ));
				float temp_output_4_0_g29 = lerpResult10_g30;
				float clampResult6_g29 = clamp( ( _Collection - temp_output_4_0_g29 ) , 0.0 , 1.0 );
				float lerpResult23_g29 = lerp( ( 1.0 - temp_output_8_0_g29 ) , ( 1.0 - temp_output_4_0_g29 ) , 0.0);
				float clampResult19_g29 = clamp( (0.0 + (clampResult6_g29 - 0.0) * (1.0 - 0.0) / (lerpResult23_g29 - 0.0)) , 0.0 , 1.0 );
				float CollectionAdjusted219 = clampResult19_g29;
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( temp_output_561_0 + ( ( float4( _CollectionPoint , 0.0 ) - temp_output_561_0 ) * CollectionAdjusted219 ) ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj26;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				o.clipPos = TransformWorldToHClip(positionWS);
				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
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
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				

				surfaceDescription.Alpha = 1;
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

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#pragma multi_compile_instancing
			#define shader_feature_local _RECEIVE_SHADOWS_OFF
			#define ASE_SRP_VERSION 140008


			#pragma vertex vert
			#pragma fragment frag

			#pragma multi_compile_fragment _ _WRITE_RENDERING_LAYERS
        	#pragma multi_compile_fragment _ _GBUFFER_NORMALS_OCT

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define VARYINGS_NEED_NORMAL_WS

			#define SHADERPASS SHADERPASS_DEPTHNORMALSONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float3 normalWS : TEXCOORD0;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _CenterColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _JitterToFusionPointScale;
			float _JitterToFusionPointSpeed;
			float _JitterToFusionPointStrength;
			float _ProgressDelay;
			float _DelayCatchUp;
			float _Progress;
			float _JitterScale;
			float _JitterScrollSpeed;
			float _JitterStrength;
			float _FusionRotationStrength;
			int _UseTime;
			float _FusionRotationVariance;
			float _FusionRotationSpeed;
			float _RotateAroundFusionZ;
			float _RotateAroundFusionY;
			float _RotateAroundFusionX;
			float _OffsetFromFusionPointVariation;
			float _OffsetFromFusionPointStrength;
			float _MoveToFusionPoint;
			float _FusionColorRange;
			float _FusionColorRangeFallOff;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform580 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 MoveToFusionPoinOffset264 = ( ( float4( FusionPoint255 , 0.0 ) - transform215 ) * _MoveToFusionPoint );
				float4 transform491 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform491.z).xx;
				float dotResult4_g113 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g113 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g113 ) * 43758.55 ) ));
				float2 temp_cast_2 = (transform491.x).xx;
				float dotResult4_g111 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g111 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g111 ) * 43758.55 ) ));
				float2 temp_cast_3 = (transform491.y).xx;
				float dotResult4_g112 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g112 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g112 ) * 43758.55 ) ));
				float2 appendResult494 = (float2(lerpResult10_g111 , lerpResult10_g112));
				float2 normalizeResult495 = normalize( appendResult494 );
				float3 appendResult499 = (float3(( _OffsetFromFusionPointStrength * ( 1.0 + ( lerpResult10_g113 * _OffsetFromFusionPointVariation ) ) * normalizeResult495 ) , 0.0));
				float3 OffsetAroundFusionPoint498 = appendResult499;
				float4 transform564 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult548 = (float3(_RotateAroundFusionX , _RotateAroundFusionY , _RotateAroundFusionZ));
				float4 transform284 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_5 = (transform284.x).xx;
				float dotResult4_g114 = dot( temp_cast_5 , float2( 12.9898,78.233 ) );
				float lerpResult10_g114 = lerp( 0.0 , _FusionRotationVariance , frac( ( sin( dotResult4_g114 ) * 43758.55 ) ));
				float temp_output_285_0 = ( _FusionRotationSpeed * ( 1.0 + lerpResult10_g114 ) );
				int UseTime545 = _UseTime;
				float mulTime42 = _TimeParameters.x * (float)UseTime545;
				float4 transform268 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 temp_output_508_0 = ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform268 );
				float3 rotatedValue16 = RotateAroundAxis( FusionPoint255, temp_output_508_0.xyz, normalize( appendResult548 ), radians( ( temp_output_285_0 + ( temp_output_285_0 * mulTime42 ) ) ) );
				float4 FusionRotationOffset250 = ( float4( rotatedValue16 , 0.0 ) - temp_output_508_0 );
				float2 appendResult324 = (float2(-_JitterStrength , _JitterStrength));
				float2 break23_g116 = appendResult324;
				float temp_output_27_0_g116 = _JitterScrollSpeed;
				float2 temp_cast_11 = (temp_output_27_0_g116).xx;
				float4 transform140 = mul(GetWorldToObjectMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_12 = (transform140.x).xx;
				float dotResult4_g104 = dot( temp_cast_12 , float2( 12.9898,78.233 ) );
				float lerpResult10_g104 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g104 ) * 43758.55 ) ));
				float2 temp_cast_13 = (transform140.y).xx;
				float dotResult4_g103 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g103 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g103 ) * 43758.55 ) ));
				float2 temp_cast_14 = (transform140.z).xx;
				float dotResult4_g102 = dot( temp_cast_14 , float2( 12.9898,78.233 ) );
				float lerpResult10_g102 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g102 ) * 43758.55 ) ));
				float4 appendResult171 = (float4(lerpResult10_g104 , lerpResult10_g103 , lerpResult10_g102 , 0.0));
				float3 temp_output_14_0_g116 = appendResult171.xyz;
				float3 break38_g116 = temp_output_14_0_g116;
				float2 appendResult39_g116 = (float2(break38_g116.y , break38_g116.z));
				float2 panner36_g116 = ( 1.0 * _Time.y * temp_cast_11 + appendResult39_g116);
				float temp_output_30_0_g116 = ( 10.0 / _JitterScale );
				float simplePerlin2D37_g116 = snoise( panner36_g116*temp_output_30_0_g116 );
				simplePerlin2D37_g116 = simplePerlin2D37_g116*0.5 + 0.5;
				float lerpResult1_g116 = lerp( break23_g116.x , break23_g116.y , simplePerlin2D37_g116);
				float2 break24_g116 = appendResult324;
				float2 temp_cast_16 = (temp_output_27_0_g116).xx;
				float3 break15_g116 = temp_output_14_0_g116;
				float2 appendResult29_g116 = (float2(break15_g116.x , break15_g116.z));
				float2 panner34_g116 = ( 1.0 * _Time.y * temp_cast_16 + appendResult29_g116);
				float simplePerlin2D6_g116 = snoise( panner34_g116*temp_output_30_0_g116 );
				simplePerlin2D6_g116 = simplePerlin2D6_g116*0.5 + 0.5;
				float lerpResult2_g116 = lerp( break24_g116.x , break24_g116.y , simplePerlin2D6_g116);
				float2 break25_g116 = appendResult324;
				float2 temp_cast_17 = (temp_output_27_0_g116).xx;
				float3 break32_g116 = temp_output_14_0_g116;
				float2 appendResult33_g116 = (float2(break32_g116.x , break32_g116.y));
				float2 panner5_g116 = ( 1.0 * _Time.y * temp_cast_17 + appendResult33_g116);
				float simplePerlin2D35_g116 = snoise( panner5_g116*temp_output_30_0_g116 );
				simplePerlin2D35_g116 = simplePerlin2D35_g116*0.5 + 0.5;
				float lerpResult3_g116 = lerp( break25_g116.x , break25_g116.y , simplePerlin2D35_g116);
				float3 appendResult10_g116 = (float3(lerpResult1_g116 , lerpResult2_g116 , lerpResult3_g116));
				float3 JitterOffset261 = appendResult10_g116;
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_19 = (transform333.x).xx;
				float dotResult4_g115 = dot( temp_cast_19 , float2( 12.9898,78.233 ) );
				float lerpResult10_g115 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g115 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g115;
				float temp_output_436_0 = ( _DelayCatchUp + _ProgressDelay );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay );
				float temp_output_567_0 = ( RandomViaX566 * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUp;
				float DelayEffect175 = saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) );
				float4 lerpResult579 = lerp( transform580 , ( ( MoveToFusionPoinOffset264 + float4( OffsetAroundFusionPoint498 , 0.0 ) + transform564 ) + ( float4( 0,0,0,0 ) + ( FusionRotationOffset250 * _FusionRotationStrength ) ) + float4( JitterOffset261 , 0.0 ) ) , DelayEffect175);
				float4 FinalPosition536 = lerpResult579;
				float2 temp_cast_21 = (( ( _JitterToFusionPointSpeed * _TimeParameters.x ) + RandomViaX566 )).xx;
				float simplePerlin2D569 = snoise( temp_cast_21*_JitterToFusionPointScale );
				simplePerlin2D569 = simplePerlin2D569*0.5 + 0.5;
				float4 temp_output_561_0 = ( lerpResult579 + ( ( float4( FusionPoint255 , 0.0 ) - FinalPosition536 ) * ( _JitterToFusionPointStrength * saturate( ( simplePerlin2D569 - 0.1 ) ) ) ) );
				float temp_output_8_0_g29 = ( _CollectionStagger * 0.5 );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_24 = (transform267.x).xx;
				float dotResult4_g30 = dot( temp_cast_24 , float2( 12.9898,78.233 ) );
				float lerpResult10_g30 = lerp( 0.0 , temp_output_8_0_g29 , frac( ( sin( dotResult4_g30 ) * 43758.55 ) ));
				float temp_output_4_0_g29 = lerpResult10_g30;
				float clampResult6_g29 = clamp( ( _Collection - temp_output_4_0_g29 ) , 0.0 , 1.0 );
				float lerpResult23_g29 = lerp( ( 1.0 - temp_output_8_0_g29 ) , ( 1.0 - temp_output_4_0_g29 ) , 0.0);
				float clampResult19_g29 = clamp( (0.0 + (clampResult6_g29 - 0.0) * (1.0 - 0.0) / (lerpResult23_g29 - 0.0)) , 0.0 , 1.0 );
				float CollectionAdjusted219 = clampResult19_g29;
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( temp_output_561_0 + ( ( float4( _CollectionPoint , 0.0 ) - temp_output_561_0 ) * CollectionAdjusted219 ) ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj26;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 normalWS = TransformObjectToWorldNormal(v.ase_normal);

				o.clipPos = TransformWorldToHClip(positionWS);
				o.normalWS.xyz =  normalWS;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
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
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			void frag( VertexOutput IN
				, out half4 outNormalWS : SV_Target0
			#ifdef _WRITE_RENDERING_LAYERS
				, out float4 outRenderingLayers : SV_Target1
			#endif
				 )
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				

				surfaceDescription.Alpha = 1;
				surfaceDescription.AlphaClipThreshold = 0.5;

				#if _ALPHATEST_ON
					clip(surfaceDescription.Alpha - surfaceDescription.AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.clipPos );
				#endif

				#if defined(_GBUFFER_NORMALS_OCT)
					float3 normalWS = normalize(IN.normalWS);
					float2 octNormalWS = PackNormalOctQuadEncode(normalWS);           // values between [-1, +1], must use fp32 on some platforms
					float2 remappedOctNormalWS = saturate(octNormalWS * 0.5 + 0.5);   // values between [ 0,  1]
					half3 packedNormalWS = PackFloat2To888(remappedOctNormalWS);      // values between [ 0,  1]
					outNormalWS = half4(packedNormalWS, 0.0);
				#else
					float3 normalWS = IN.normalWS;
					outNormalWS = half4(NormalizeNormalPerPixel(normalWS), 0.0);
				#endif

				#ifdef _WRITE_RENDERING_LAYERS
					uint renderingLayers = GetMeshRenderingLayer();
					outRenderingLayers = float4(EncodeMeshRenderingLayer(renderingLayers), 0, 0, 0);
				#endif
			}

			ENDHLSL
		}

		/*ase_pass*/
		Pass
		{
			PackageRequirements
			{
				"com.unity.render-pipelines.universal": "unity=[2022.3.10,2022.3.45]"
			}

			
			Name "GBuffer"
			Tags 
			{
				"LightMode" = "UniversalGBuffer" 
			}

			HLSLPROGRAM
			/*ase_pragma_before*/
			#pragma exclude_renderers gles3 glcore

			#pragma vertex UnlitPassVertex
			#pragma fragment UnlitPassFragment

			//#pragma shader_feature_local_fragment _ALPHATEST_ON
			//#pragma shader_feature_local_fragment _ALPHAMODULATE_ON

			//#pragma multi_compile_fragment _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile_fragment _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"

			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"

			#include "Packages/com.unity.render-pipelines.universal/Shaders/UnlitInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Shaders/UnlitGBufferPass.hlsl"

			/*ase_pragma*/

			/*ase_globals*/

			/*ase_funcs*/

			ENDHLSL
		}
		
	}
	
	CustomEditor "UnityEditor.ShaderGraphUnlitGUI"
	FallBack "Hidden/Shader Graph/FallbackError"
	
	Fallback Off
}
/*ASEBEGIN
Version=19201
Node;AmplifyShaderEditor.CommentaryNode;590;6759.233,-932.1898;Inherit;False;742.2939;484.9873;vertices;6;373;371;372;374;562;563;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;565;6286.187,-3419.004;Inherit;False;1577.871;854.5828;Comment;13;516;532;537;519;539;540;538;529;526;533;525;531;515;CenterColor;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;459;-2298.636,198.3543;Inherit;False;2744.508;923.9855;Comment;27;2;3;4;5;6;7;8;9;440;434;333;175;449;330;436;439;435;458;452;129;447;455;441;438;426;450;566;Delay;0.6597723,0.2776922,0.8773585,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;351;1072.427,-1393.301;Inherit;False;883.9728;539.3771;;0;Jitter Mixer;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;265;679.2864,-3042.583;Inherit;False;1103.841;648.4066;;6;194;255;197;215;393;395;Move To Fusion Point;0.6653691,0.8490566,0.6127626,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;174;-1475.45,-2600.533;Inherit;False;2005.256;728.2788;;12;261;68;69;140;172;173;139;171;325;402;324;249;Jitter;0.1294118,0.3764706,0.3357446,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;62;-275.7516,1396.879;Inherit;False;996.7706;874.191;;8;55;85;87;57;54;59;58;53;Position Variance;0.2973033,0.4811321,0.4607901,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;48;4440.478,-501.3985;Inherit;False;2212.742;1278.682;Rotation;24;283;285;42;250;268;281;0;256;47;16;284;290;291;101;506;507;508;543;546;547;548;551;550;549;Rotate Around Fusion Point;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;58;-3.23881,1661.985;Inherit;False;Constant;_Float0;Float 0;4;0;Create;True;0;0;0;False;0;False;-1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;87;116.7471,2004.139;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;85;-221.6876,2013.538;Inherit;False;Property;_EndPositionVariance;EndPositionVariance;9;0;Create;True;0;0;0;False;0;False;0,0,0;0.2,0.2,0.2;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.Vector3Node;55;-138.0756,1790.123;Inherit;False;Property;_StartPositionVariance;StartPositionVariance;8;0;Create;True;0;0;0;False;0;False;0,0,0;2,2,2;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;57;255.638,1787.375;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;54;-34.75785,1561.984;Inherit;True;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;59;437.9031,1976.874;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.WorldToObjectTransfNode;53;-225.9337,1434.455;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;5670.049,-445.8002;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.FunctionNode;390;-2830.746,-196.2491;Inherit;False;Stagger;-1;;29;93a439cb4f13e644e8dcf460c2df1f83;0;4;24;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;11;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;267;-3159.375,-35.53355;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;395;1478.34,-2774.906;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;171;-778.1138,-2468.914;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.NegateNode;325;-783.2282,-2186.046;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;173;-1013.333,-2271.606;Inherit;False;Random Range;-1;;102;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;172;-1011.865,-2394.277;Inherit;False;Random Range;-1;;103;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldToObjectTransfNode;140;-1392.019,-2496.539;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;69;-607.5696,-2574.582;Inherit;False;Property;_JitterScrollSpeed;JitterScrollSpeed;12;0;Create;True;0;0;0;False;0;False;0;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;68;-578.6753,-2476.967;Inherit;False;Property;_JitterScale;JitterScale;11;0;Create;True;0;0;0;False;0;False;0;12.74;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;139;-1010.383,-2534.597;Inherit;False;Random Range;-1;;104;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;411;-1868.747,-1797.16;Inherit;False;2005.256;728.2788;;11;423;422;420;419;418;417;416;415;414;413;412;Jitter;0.1294118,0.3764706,0.3357446,1;0;0
Node;AmplifyShaderEditor.DynamicAppendNode;412;-1171.411,-1665.541;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;413;-967.8831,-1335.821;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.NegateNode;414;-1176.526,-1382.673;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;415;-1406.631,-1468.233;Inherit;False;Random Range;-1;;106;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;416;-1405.163,-1590.904;Inherit;False;Random Range;-1;;107;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldToObjectTransfNode;417;-1785.316,-1693.166;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;418;-1000.867,-1771.209;Inherit;False;Property;_Float3;Float 3;13;0;Create;True;0;0;0;False;0;False;0;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;419;-971.973,-1673.594;Inherit;False;Property;_Float4;Float 4;10;0;Create;True;0;0;0;False;0;False;0;12.74;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;422;-1403.681,-1731.224;Inherit;False;Random Range;-1;;108;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;423;-610.0724,-1678.795;Inherit;True;Waves;-1;;109;52c727e79d995054d85e9a21c58a69db;0;6;27;FLOAT;0;False;26;FLOAT;0;False;20;FLOAT2;0,0;False;21;FLOAT2;0,0;False;22;FLOAT2;0,0;False;14;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;324;-571.9731,-2309.649;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;420;-1517.413,-1289.063;Inherit;False;Property;_Float5;Float 5;18;0;Create;True;0;0;0;False;0;False;0;0;0;0.5;0;1;FLOAT;0
Node;AmplifyShaderEditor.StickyNoteNode;440;-1500.782,248.3543;Inherit;False;228.8146;130.6501;;;1,1,1,1;Make sure that delay + catchup is within one, otherwise some particles might not catch up before the end.$;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;8;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;9;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.RangedFloatNode;434;-2209.402,423.7738;Inherit;False;Property;_DelayCatchUp;DelayCatchUp;15;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;449;-1854.257,280.0054;Inherit;False;DelayCatchUp;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;330;-2248.636,522.6386;Inherit;False;Property;_ProgressDelay;ProgressDelay;16;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;436;-1756.424,368.7398;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;439;-1762.944,646.3052;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;435;-1535.777,491.4302;Inherit;False;2;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;458;-1886.365,774.2071;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;491;1902.502,-2479.379;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;494;2370.502,-2305.379;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.NormalizeNode;495;2552.502,-2270.379;Inherit;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;497;2648.052,-2160.392;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;499;2965.699,-2243.404;Inherit;False;FLOAT3;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;498;3116.968,-2209.024;Inherit;False;OffsetAroundFusionPoint;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;496;2777.618,-2298.911;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;492;2176.502,-2394.379;Inherit;False;Random Range;-1;;111;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;-1;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;503;2475.516,-2152.135;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;493;2177.502,-2244.379;Inherit;False;Random Range;-1;;112;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;-1;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;504;2181.516,-2093.135;Inherit;False;Random Range;-1;;113;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RadiansOpNode;47;5536.7,-189.4543;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;175;228.9544,628.8358;Inherit;False;DelayEffect;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;452;-18.57532,532.9445;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;129;-925.8953,353.4683;Inherit;False;Property;_Progress;Progress;0;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;447;-429.4876,691.6467;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;455;-691.8315,727.3923;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;441;-221.4355,514.4973;Inherit;False;3;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;450;-780.4482,959.3747;Inherit;False;449;DelayCatchUp;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;541;-2939.541,-523.5134;Inherit;False;Property;_UseTime;UseTime;29;0;Create;True;0;0;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;545;-2645.703,-486.8243;Inherit;False;UseTime;-1;True;1;0;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;284;4476.817,-451.2172;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;101;4452.944,-230.95;Inherit;False;Property;_FusionRotationVariance;FusionRotationVariance;6;0;Create;True;0;0;0;False;0;False;10;1.2;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;290;4693.069,-403.6466;Inherit;False;Random Range;-1;;114;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;291;4923.63,-295.547;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;42;4996.751,-135.1125;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;543;5300.809,-110.6287;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;546;5437.489,-344.3877;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;285;5139.504,-301.9152;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;547;4781.896,-132.2116;Inherit;False;545;UseTime;1;0;OBJECT;;False;1;INT;0
Node;AmplifyShaderEditor.RangedFloatNode;283;4876.572,-455.4573;Inherit;False;Property;_FusionRotationSpeed;FusionRotationSpeed;19;0;Create;True;0;0;0;False;0;False;0;0;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;508;5597.285,354.3374;Inherit;False;3;3;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;268;5354.583,569.1437;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleSubtractOpNode;281;6273.907,546.6948;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;548;6082.606,-178.4054;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;551;5773.98,-114.8216;Inherit;False;Property;_RotateAroundFusionZ;RotateAroundFusionZ;28;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;550;5738.876,-233.278;Inherit;False;Property;_RotateAroundFusionY;RotateAroundFusionY;27;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;264;1836.324,-2624.241;Inherit;False;MoveToFusionPoinOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;393;1159.46,-2545.419;Inherit;False;Property;_MoveToFusionPoint;MoveToFusionPoint;21;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;194;716.2864,-2987.546;Inherit;False;Property;_FusionPoint;FusionPoint;2;0;Create;False;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;255;951.4233,-2988.163;Inherit;False;FusionPoint;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;215;851.243,-2692.365;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleSubtractOpNode;197;1137.502,-2803.378;Inherit;True;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DistanceOpNode;516;6582.847,-3238.978;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;532;6645.954,-2896.384;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;537;6378.489,-3143.004;Inherit;False;536;FinalPosition;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;519;6336.187,-2995.24;Inherit;False;Property;_FusionColorRange;FusionColorRange;24;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;539;6922.979,-3149.436;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;540;7232.979,-2994.436;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;538;7102.979,-3216.436;Inherit;False;2;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;529;7396.417,-3039.532;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;526;7590.058,-2891.81;Inherit;False;FusionColorAmount;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;533;7046.899,-2992.499;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;525;6457.657,-2768.81;Inherit;False;Property;_FusionColorRangeFallOff;FusionColorRangeFallOff;25;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;531;6837.188,-2772.421;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;515;6370.941,-3369.004;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;438;-1278.463,824.7924;Inherit;False;DelayCapped;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;333;-1344.326,468.0655;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;426;-1210.055,651.8334;Inherit;False;Random Range;-1;;115;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;566;-1062.404,526.3585;Inherit;False;RandomViaX;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;567;-997.4038,804.3585;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;523;6982.394,-1749.224;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;10;6672.854,-1999.508;Inherit;False;Property;_MainColor;MainColor;5;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,1.017304,5.934532,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;518;6622.454,-1790.491;Inherit;False;Property;_CenterColor;CenterColor;4;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,1.017304,5.934532,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;527;6605.212,-1593.123;Inherit;False;526;FusionColorAmount;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;557;5445.635,-1936.074;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;568;5210.922,-2115.129;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;577;5092.241,-2253.073;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;578;5347.241,-2265.073;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;502;4242.984,-1652.67;Inherit;False;498;OffsetAroundFusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;399;4231.954,-1029.331;Inherit;False;Property;_FusionRotationStrength;FusionRotationStrength;20;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;400;4612.75,-1163.665;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;509;4275.465,-1206.94;Inherit;False;250;FusionRotationOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;488;4985.729,-1185.435;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;564;4329.152,-1473.954;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;209;4680.095,-1611.281;Inherit;False;3;3;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;266;4316.193,-1757.688;Inherit;False;264;MoveToFusionPoinOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;507;5246.648,408.6564;Inherit;False;498;OffsetAroundFusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;506;5241.414,266.4676;Inherit;False;264;MoveToFusionPoinOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;16;5992.373,232.3379;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;256;5610.686,171.6582;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;250;6421.983,-102.298;Inherit;False;FusionRotationOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;559;5627.588,-1281.51;Inherit;False;3;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT3;0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;556;5226.221,-1978.472;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;581;5230.873,-1871.036;Inherit;False;536;FinalPosition;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;570;5436.43,-2436.471;Inherit;False;Property;_JitterToFusionPointScale;JitterToFusionPointScale;32;0;Create;True;0;0;0;False;0;False;7.46;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;572;5059.905,-2380.405;Inherit;False;Property;_JitterToFusionPointSpeed;JitterToFusionPointSpeed;31;0;Create;True;0;0;0;False;0;False;0.3247919;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;560;6071.78,-1817.844;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;571;6184.852,-2303.826;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;584;5980.979,-2259.089;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;490;2176.518,-1955.905;Inherit;False;Property;_OffsetFromFusionPointVariation;OffsetFromFusionPointVariation;22;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;489;2431.673,-2487.822;Inherit;False;Property;_OffsetFromFusionPointStrength;OffsetFromFusionPointStrength;23;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;575;5484.56,-2169.118;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;569;5721.943,-2077.602;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;585;5888.341,-2174.934;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;249;-1302.221,-2139.688;Inherit;False;Property;_JitterStrength;JitterStrength;17;0;Create;True;0;0;0;False;0;False;0;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;402;-216.7753,-2482.168;Inherit;True;Waves;-1;;116;52c727e79d995054d85e9a21c58a69db;0;6;27;FLOAT;0;False;26;FLOAT;0;False;20;FLOAT2;0,0;False;21;FLOAT2;0,0;False;22;FLOAT2;0,0;False;14;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;261;224.6624,-2485.879;Inherit;False;JitterOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;586;5530.65,-1535.767;Inherit;False;261;JitterOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;558;5663.988,-2482.434;Inherit;False;Property;_JitterToFusionPointStrength;JitterToFusionPointStrength;30;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;549;5701.671,-350.8521;Inherit;False;Property;_RotateAroundFusionX;RotateAroundFusionX;26;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;374;6809.233,-563.8953;Inherit;False;Property;_ParticleScale;ParticleScale;7;0;Create;True;0;0;0;False;0;False;0.3;0.2;0;0.3;0;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;372;6781.522,-879.5408;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;371;7027.239,-752.1393;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;580;5976.855,-1626.253;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;392;5992.875,-1199.848;Inherit;False;175;DelayEffect;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;579;6206.276,-1343.612;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TransformPositionNode;373;7220.757,-883.5905;Inherit;True;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;7544.213,-1482;Float;False;True;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;Potato;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;1;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;;0;0;Standard;23;Surface;0;0;  Blend;0;0;Two Sided;1;0;Forward Only;0;0;Cast Shadows;0;638332276799351495;  Use Shadow Threshold;0;0;Receive Shadows;0;638332276827847575;GPU Instancing;1;638332271287541535;LOD CrossFade;0;0;Built-in Fog;0;0;DOTS Instancing;0;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Vertex Position,InvertActionOnDeselection;0;638274534278721169;0;10;False;True;False;True;False;False;True;True;True;False;False;;False;0
Node;AmplifyShaderEditor.TransformPositionNode;26;7273.738,-1394.993;Inherit;False;World;Object;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;589;7119.43,-1390.484;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;562;7129.24,-610.0544;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleSubtractOpNode;563;7369.612,-648.5915;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;536;6535.328,-1505.208;Inherit;False;FinalPosition;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;594;6872.043,-1358.608;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;593;6642.124,-1214.277;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;595;6804.916,-1244.485;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;561;6480.813,-1363.493;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector3Node;225;6345.202,-1200.59;Inherit;False;Property;_CollectionPoint;CollectionPoint;3;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;230;-3208.792,-222.6347;Inherit;False;Property;_CollectionStagger;CollectionStagger;14;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;218;-3265.945,-138.8559;Inherit;False;Property;_Collection;Collection;1;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;219;-2551.975,-166.7117;Inherit;False;CollectionAdjusted;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;596;6552.553,-1053.922;Inherit;False;219;CollectionAdjusted;1;0;OBJECT;;False;1;FLOAT;0
WireConnection;87;0;55;0
WireConnection;87;1;85;0
WireConnection;57;0;58;0
WireConnection;57;1;87;0
WireConnection;54;0;53;0
WireConnection;59;0;57;0
WireConnection;59;1;87;0
WireConnection;59;2;54;0
WireConnection;390;9;230;0
WireConnection;390;10;218;0
WireConnection;390;11;267;1
WireConnection;395;0;197;0
WireConnection;395;1;393;0
WireConnection;171;0;139;0
WireConnection;171;1;172;0
WireConnection;171;2;173;0
WireConnection;325;0;249;0
WireConnection;173;1;140;3
WireConnection;172;1;140;2
WireConnection;139;1;140;1
WireConnection;412;0;422;0
WireConnection;412;1;416;0
WireConnection;412;2;415;0
WireConnection;413;0;414;0
WireConnection;413;1;420;0
WireConnection;414;0;420;0
WireConnection;415;1;417;3
WireConnection;416;1;417;2
WireConnection;422;1;417;1
WireConnection;423;27;418;0
WireConnection;423;26;419;0
WireConnection;423;20;413;0
WireConnection;423;21;413;0
WireConnection;423;22;413;0
WireConnection;423;14;412;0
WireConnection;324;0;325;0
WireConnection;324;1;249;0
WireConnection;449;0;434;0
WireConnection;436;0;434;0
WireConnection;436;1;330;0
WireConnection;439;0;330;0
WireConnection;439;1;458;0
WireConnection;435;0;436;0
WireConnection;435;2;439;0
WireConnection;435;3;330;0
WireConnection;458;0;436;0
WireConnection;494;0;492;0
WireConnection;494;1;493;0
WireConnection;495;0;494;0
WireConnection;497;1;503;0
WireConnection;499;0;496;0
WireConnection;498;0;499;0
WireConnection;496;0;489;0
WireConnection;496;1;497;0
WireConnection;496;2;495;0
WireConnection;492;1;491;1
WireConnection;503;0;504;0
WireConnection;503;1;490;0
WireConnection;493;1;491;2
WireConnection;504;1;491;3
WireConnection;47;0;546;0
WireConnection;175;0;452;0
WireConnection;452;0;441;0
WireConnection;447;0;455;0
WireConnection;447;2;450;0
WireConnection;455;0;129;0
WireConnection;455;1;567;0
WireConnection;441;0;129;0
WireConnection;441;1;567;0
WireConnection;441;2;447;0
WireConnection;545;0;541;0
WireConnection;290;1;284;1
WireConnection;290;3;101;0
WireConnection;291;1;290;0
WireConnection;42;0;547;0
WireConnection;543;0;285;0
WireConnection;543;1;42;0
WireConnection;546;0;285;0
WireConnection;546;1;543;0
WireConnection;285;0;283;0
WireConnection;285;1;291;0
WireConnection;508;0;506;0
WireConnection;508;1;507;0
WireConnection;508;2;268;0
WireConnection;281;0;16;0
WireConnection;281;1;508;0
WireConnection;548;0;549;0
WireConnection;548;1;550;0
WireConnection;548;2;551;0
WireConnection;264;0;395;0
WireConnection;255;0;194;0
WireConnection;197;0;255;0
WireConnection;197;1;215;0
WireConnection;516;0;515;0
WireConnection;516;1;537;0
WireConnection;532;0;516;0
WireConnection;532;1;519;0
WireConnection;539;0;519;0
WireConnection;539;1;525;0
WireConnection;540;0;533;0
WireConnection;540;1;538;0
WireConnection;538;0;516;0
WireConnection;538;1;539;0
WireConnection;529;0;540;0
WireConnection;526;0;529;0
WireConnection;533;0;516;0
WireConnection;533;1;519;0
WireConnection;533;3;531;0
WireConnection;531;0;532;0
WireConnection;531;2;525;0
WireConnection;438;0;435;0
WireConnection;426;1;333;1
WireConnection;566;0;426;0
WireConnection;567;0;566;0
WireConnection;567;1;438;0
WireConnection;523;0;10;0
WireConnection;523;1;518;0
WireConnection;523;2;527;0
WireConnection;557;0;556;0
WireConnection;557;1;581;0
WireConnection;578;0;572;0
WireConnection;578;1;577;0
WireConnection;400;0;509;0
WireConnection;400;1;399;0
WireConnection;488;1;400;0
WireConnection;209;0;266;0
WireConnection;209;1;502;0
WireConnection;209;2;564;0
WireConnection;16;0;548;0
WireConnection;16;1;47;0
WireConnection;16;2;256;0
WireConnection;16;3;508;0
WireConnection;250;0;281;0
WireConnection;559;0;209;0
WireConnection;559;1;488;0
WireConnection;559;2;586;0
WireConnection;560;0;557;0
WireConnection;560;1;571;0
WireConnection;571;0;558;0
WireConnection;571;1;584;0
WireConnection;584;0;585;0
WireConnection;575;0;578;0
WireConnection;575;1;568;0
WireConnection;569;0;575;0
WireConnection;569;1;570;0
WireConnection;585;0;569;0
WireConnection;402;27;69;0
WireConnection;402;26;68;0
WireConnection;402;20;324;0
WireConnection;402;21;324;0
WireConnection;402;22;324;0
WireConnection;402;14;171;0
WireConnection;261;0;402;0
WireConnection;371;0;372;0
WireConnection;371;1;374;0
WireConnection;579;0;580;0
WireConnection;579;1;559;0
WireConnection;579;2;392;0
WireConnection;373;0;371;0
WireConnection;1;2;523;0
WireConnection;1;5;26;0
WireConnection;26;0;589;0
WireConnection;589;0;594;0
WireConnection;589;1;563;0
WireConnection;563;0;373;0
WireConnection;563;1;562;0
WireConnection;536;0;579;0
WireConnection;594;0;561;0
WireConnection;594;1;595;0
WireConnection;593;0;225;0
WireConnection;593;1;561;0
WireConnection;595;0;593;0
WireConnection;595;1;596;0
WireConnection;561;0;579;0
WireConnection;561;1;560;0
WireConnection;219;0;390;0
ASEEND*/
//CHKSM=C8AFF9EFAB1DD58B2D9D137EF45CC46747A59F90