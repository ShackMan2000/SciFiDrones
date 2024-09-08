// Made with Amplify Shader Editor v1.9.2.1
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Potato 3 Axis Rotation Fusion"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_UseTime("UseTime", Int) = 0
		_Transformation("Transformation", Range( 0 , 1)) = 0
		_ProgressDelay_Min("ProgressDelay_Min", Range( 0 , 1)) = 0
		_ParticleScale("ParticleScale", Range( 0 , 0.3)) = 0.3
		[HDR]_MainColor("MainColor", Color) = (0.2042542,0.5284038,0.9622642,0)
		[HDR]_FusionAxisColor("FusionAxisColor", Color) = (0.2042542,0.5284038,0.9622642,0)
		_FusionAxisColor_MinDistance("FusionAxisColor_MinDistance", Float) = 0
		_FusionAxisColor_MaxDistance("FusionAxisColor_MaxDistance", Float) = 0
		_ProgressDelay_Variance("ProgressDelay_Variance", Range( 0 , 1)) = 1
		_DelayCatchUpTime("DelayCatchUpTime", Range( 0 , 1)) = 1
		_RotateY_Speed("RotateY_Speed", Range( 0 , 500)) = 0
		_RotateY_Variance("RotateY_Variance", Range( 0 , 10)) = 10
		_RotateZ_Speed("RotateZ_Speed", Range( 0 , 500)) = 0
		_RotateZ_Variance("RotateZ_Variance", Range( 0 , 10)) = 10
		_FusionPointRingOffsetStrength("FusionPointRingOffsetStrength", Range( 0 , 1)) = 0.0434969
		_FusionPointOnAxisOffset("FusionPointOnAxisOffset", Range( 0 , 1)) = 0.0434969
		_FusionPointRingOffsetVariance("FusionPointRingOffsetVariance", Range( 0 , 2.17)) = 0
		_FusionPoint("FusionPoint", Vector) = (0,0,0,0)
		_ExplosionTime("ExplosionTime", Range( 0 , 0.5)) = 0
		_MaxFusionExpansion("MaxFusionExpansion", Range( 0 , 1)) = 1
		_FusionMulti("FusionMulti", Range( 1 , 10)) = 1
		_Fusion("Fusion", Range( 0 , 1)) = 0
		_ExplosionRadius("ExplosionRadius", Range( 0 , 5)) = 0
		_ExplosionRotationSpeed("ExplosionRotationSpeed", Range( -2.02 , 3.54)) = 1
		_FusionJitterStrength("FusionJitterStrength", Float) = 0
		_FusionJitterFrequencyVariance("FusionJitterFrequencyVariance", Float) = 0
		_FusionJitterFrequency("FusionJitterFrequency", Float) = 0
		_FusionRotationStrength("FusionRotationStrength", Range( 0 , 3)) = 0
		_Collection("Collection", Range( 0 , 1)) = 0
		_CollectionPoint("CollectionPoint", Vector) = (0,0,0,0)
		_StartCollectionAt("StartCollectionAt", Range( 0 , 0.99)) = 0
		_CollectionTime("CollectionTime", Range( 0 , 0.5)) = 0.044


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
			#define ASE_SRP_VERSION 140009


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
			float4 _FusionAxisColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionTime;
			float _StartCollectionAt;
			float _ExplosionTime;
			float _FusionMulti;
			float _Collection;
			float _ExplosionRadius;
			float _ExplosionRotationSpeed;
			float _MaxFusionExpansion;
			float _Fusion;
			int _UseTime;
			float _FusionJitterFrequency;
			float _FusionJitterFrequencyVariance;
			float _FusionJitterStrength;
			float _FusionRotationStrength;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Transformation;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _FusionPointOnAxisOffset;
			float _FusionPointRingOffsetVariance;
			float _FusionPointRingOffsetStrength;
			float _FusionAxisColor_MinDistance;
			float _FusionAxisColor_MaxDistance;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			real2 ASESafeNormalize(float2 inVec)
			{
				real dp3 = max(FLT_MIN, dot(inVec, inVec));
				return inVec* rsqrt( dp3);
			}
			
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
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 MovedToFusionPoint264 = _FusionPoint;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g135 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g135 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g135 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g135;
				float temp_output_651_0 = ( ( RandomViaY641 - 0.5 ) * 2.0 );
				float2 appendResult638 = (float2(temp_output_650_0 , temp_output_651_0));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_3 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength * ( 1.0 + ( RandomViaX566 * _FusionPointRingOffsetVariance ) ) );
				float2 temp_cast_4 = (RandomViaZ656).xx;
				float dotResult4_g123 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g123 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g123 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g123 ) , 0.0));
				float4 FusionPositionWithVariance880 = ( float4( MovedToFusionPoint264 , 0.0 ) + appendResult883 );
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float3 rotatedValue785 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - FusionPositionWithVariance880 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedZAroundFusionPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - FusionPositionWithVariance880 );
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Transformation * saturate( ( _Transformation >= temp_output_567_0 ? (0.0 + (( _Transformation - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float4 lerpResult863 = lerp( transform872 , ( FusionPositionWithVariance880 + RotatedYAroundClosesAxisPointOffset790 + RotatedZAroundFusionPointOffset818 ) , Progress_Delayed619);
				float temp_output_963_0 = ( RandomViaZ656 - 0.5 );
				float temp_output_961_0 = ( RandomViaX566 - 0.5 );
				float temp_output_962_0 = ( RandomViaY641 - 0.5 );
				float3 appendResult975 = (float3(temp_output_963_0 , temp_output_961_0 , temp_output_962_0));
				float mulTime970 = _TimeParameters.x * ( _FusionRotationStrength + temp_output_962_0 );
				float3 appendResult953 = (float3(temp_output_961_0 , temp_output_962_0 , temp_output_963_0));
				float3 normalizeResult954 = normalize( appendResult953 );
				float2 temp_cast_9 = (RandomViaY641).xx;
				float dotResult4_g133 = dot( temp_cast_9 , float2( 12.9898,78.233 ) );
				float lerpResult10_g133 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g133 ) * 43758.55 ) ));
				int UseTime545 = _UseTime;
				float mulTime957 = _TimeParameters.x * (float)UseTime545;
				float temp_output_958_0 = sin( ( ( _FusionJitterFrequencyVariance * lerpResult10_g133 ) + ( _FusionJitterFrequency + ( _FusionJitterFrequency * mulTime957 ) ) ) );
				float3 rotatedValue968 = RotateAroundAxis( FusionPoint255, ( FusionPoint255 + ( normalizeResult954 * _FusionJitterStrength * temp_output_958_0 ) ), normalize( appendResult975 ), mulTime970 );
				float3 FusionPointWithNoise1065 = rotatedValue968;
				float temp_output_1062_0 = (_MaxFusionExpansion + (temp_output_958_0 - -1.0) * (1.0 - _MaxFusionExpansion) / (1.0 - -1.0));
				float MoveToFusionPoint1060 = temp_output_1062_0;
				float4 lerpResult899 = lerp( lerpResult863 , float4( FusionPointWithNoise1065 , 0.0 ) , ( _Fusion * MoveToFusionPoint1060 ));
				float3 appendResult997 = (float3(RandomViaZ656 , RandomViaY641 , RandomViaX566));
				float mulTime996 = _TimeParameters.x * _ExplosionRotationSpeed;
				float4 appendResult987 = (float4(( RandomViaX566 - 0.5 ) , ( RandomViaY641 - 0.5 ) , ( RandomViaZ656 - 0.5 ) , 0.0));
				float4 normalizeResult988 = normalize( appendResult987 );
				float CollectionAdjusted219 = _Collection;
				float2 temp_cast_13 = (RandomViaX566).xx;
				float dotResult4_g136 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g136 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g136 ) * 43758.55 ) ));
				float temp_output_1056_0 = ( floor( ( lerpResult10_g136 * _FusionMulti ) ) * _ExplosionTime );
				float3 rotatedValue995 = RotateAroundAxis( FusionPoint255, ( float4( FusionPoint255 , 0.0 ) + ( _ExplosionRadius * normalizeResult988 * sin( ( ( 0.5 * PI ) * saturate( (0.0 + (CollectionAdjusted219 - temp_output_1056_0) * (1.0 - 0.0) / (( temp_output_1056_0 + _ExplosionTime ) - temp_output_1056_0)) ) ) ) ) ).xyz, normalize( appendResult997 ), mulTime996 );
				float3 PositionAfterExplosion999 = rotatedValue995;
				float4 lerpResult982 = lerp( lerpResult899 , float4( PositionAfterExplosion999 , 0.0 ) , ( CollectionAdjusted219 > 0.001 ? 1.0 : 0.0 ));
				float collectionDelay1036 = ( ( ( 1.0 - _StartCollectionAt ) - _CollectionTime ) * RandomViaX566 );
				float MoveToCollectionPoint1043 = saturate( (0.0 + (CollectionAdjusted219 - ( _StartCollectionAt + collectionDelay1036 )) * (1.0 - 0.0) / (( ( _StartCollectionAt + _CollectionTime ) + collectionDelay1036 ) - ( _StartCollectionAt + collectionDelay1036 ))) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , MoveToCollectionPoint1043);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( lerpResult1045 + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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
				float3 ClosestPointOnFusionAxis615 = FusionPoint255;
				float temp_output_861_0 = ( distance( ClosestPointOnFusionAxis615 , half3(0,0,0) ) + distance( FusionPoint255 , half3(0,0,0) ) );
				float FusionColorAmount526 = saturate( (1.0 + (temp_output_861_0 - _FusionAxisColor_MinDistance) * (0.0 - 1.0) / (_FusionAxisColor_MaxDistance - _FusionAxisColor_MinDistance)) );
				float4 lerpResult523 = lerp( _MainColor , _FusionAxisColor , FusionColorAmount526);
				
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
			#define ASE_SRP_VERSION 140009


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
			float4 _FusionAxisColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionTime;
			float _StartCollectionAt;
			float _ExplosionTime;
			float _FusionMulti;
			float _Collection;
			float _ExplosionRadius;
			float _ExplosionRotationSpeed;
			float _MaxFusionExpansion;
			float _Fusion;
			int _UseTime;
			float _FusionJitterFrequency;
			float _FusionJitterFrequencyVariance;
			float _FusionJitterStrength;
			float _FusionRotationStrength;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Transformation;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _FusionPointOnAxisOffset;
			float _FusionPointRingOffsetVariance;
			float _FusionPointRingOffsetStrength;
			float _FusionAxisColor_MinDistance;
			float _FusionAxisColor_MaxDistance;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			real2 ASESafeNormalize(float2 inVec)
			{
				real dp3 = max(FLT_MIN, dot(inVec, inVec));
				return inVec* rsqrt( dp3);
			}
			
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
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 MovedToFusionPoint264 = _FusionPoint;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g135 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g135 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g135 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g135;
				float temp_output_651_0 = ( ( RandomViaY641 - 0.5 ) * 2.0 );
				float2 appendResult638 = (float2(temp_output_650_0 , temp_output_651_0));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_3 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength * ( 1.0 + ( RandomViaX566 * _FusionPointRingOffsetVariance ) ) );
				float2 temp_cast_4 = (RandomViaZ656).xx;
				float dotResult4_g123 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g123 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g123 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g123 ) , 0.0));
				float4 FusionPositionWithVariance880 = ( float4( MovedToFusionPoint264 , 0.0 ) + appendResult883 );
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float3 rotatedValue785 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - FusionPositionWithVariance880 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedZAroundFusionPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - FusionPositionWithVariance880 );
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Transformation * saturate( ( _Transformation >= temp_output_567_0 ? (0.0 + (( _Transformation - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float4 lerpResult863 = lerp( transform872 , ( FusionPositionWithVariance880 + RotatedYAroundClosesAxisPointOffset790 + RotatedZAroundFusionPointOffset818 ) , Progress_Delayed619);
				float temp_output_963_0 = ( RandomViaZ656 - 0.5 );
				float temp_output_961_0 = ( RandomViaX566 - 0.5 );
				float temp_output_962_0 = ( RandomViaY641 - 0.5 );
				float3 appendResult975 = (float3(temp_output_963_0 , temp_output_961_0 , temp_output_962_0));
				float mulTime970 = _TimeParameters.x * ( _FusionRotationStrength + temp_output_962_0 );
				float3 appendResult953 = (float3(temp_output_961_0 , temp_output_962_0 , temp_output_963_0));
				float3 normalizeResult954 = normalize( appendResult953 );
				float2 temp_cast_9 = (RandomViaY641).xx;
				float dotResult4_g133 = dot( temp_cast_9 , float2( 12.9898,78.233 ) );
				float lerpResult10_g133 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g133 ) * 43758.55 ) ));
				int UseTime545 = _UseTime;
				float mulTime957 = _TimeParameters.x * (float)UseTime545;
				float temp_output_958_0 = sin( ( ( _FusionJitterFrequencyVariance * lerpResult10_g133 ) + ( _FusionJitterFrequency + ( _FusionJitterFrequency * mulTime957 ) ) ) );
				float3 rotatedValue968 = RotateAroundAxis( FusionPoint255, ( FusionPoint255 + ( normalizeResult954 * _FusionJitterStrength * temp_output_958_0 ) ), normalize( appendResult975 ), mulTime970 );
				float3 FusionPointWithNoise1065 = rotatedValue968;
				float temp_output_1062_0 = (_MaxFusionExpansion + (temp_output_958_0 - -1.0) * (1.0 - _MaxFusionExpansion) / (1.0 - -1.0));
				float MoveToFusionPoint1060 = temp_output_1062_0;
				float4 lerpResult899 = lerp( lerpResult863 , float4( FusionPointWithNoise1065 , 0.0 ) , ( _Fusion * MoveToFusionPoint1060 ));
				float3 appendResult997 = (float3(RandomViaZ656 , RandomViaY641 , RandomViaX566));
				float mulTime996 = _TimeParameters.x * _ExplosionRotationSpeed;
				float4 appendResult987 = (float4(( RandomViaX566 - 0.5 ) , ( RandomViaY641 - 0.5 ) , ( RandomViaZ656 - 0.5 ) , 0.0));
				float4 normalizeResult988 = normalize( appendResult987 );
				float CollectionAdjusted219 = _Collection;
				float2 temp_cast_13 = (RandomViaX566).xx;
				float dotResult4_g136 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g136 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g136 ) * 43758.55 ) ));
				float temp_output_1056_0 = ( floor( ( lerpResult10_g136 * _FusionMulti ) ) * _ExplosionTime );
				float3 rotatedValue995 = RotateAroundAxis( FusionPoint255, ( float4( FusionPoint255 , 0.0 ) + ( _ExplosionRadius * normalizeResult988 * sin( ( ( 0.5 * PI ) * saturate( (0.0 + (CollectionAdjusted219 - temp_output_1056_0) * (1.0 - 0.0) / (( temp_output_1056_0 + _ExplosionTime ) - temp_output_1056_0)) ) ) ) ) ).xyz, normalize( appendResult997 ), mulTime996 );
				float3 PositionAfterExplosion999 = rotatedValue995;
				float4 lerpResult982 = lerp( lerpResult899 , float4( PositionAfterExplosion999 , 0.0 ) , ( CollectionAdjusted219 > 0.001 ? 1.0 : 0.0 ));
				float collectionDelay1036 = ( ( ( 1.0 - _StartCollectionAt ) - _CollectionTime ) * RandomViaX566 );
				float MoveToCollectionPoint1043 = saturate( (0.0 + (CollectionAdjusted219 - ( _StartCollectionAt + collectionDelay1036 )) * (1.0 - 0.0) / (( ( _StartCollectionAt + _CollectionTime ) + collectionDelay1036 ) - ( _StartCollectionAt + collectionDelay1036 ))) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , MoveToCollectionPoint1043);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( lerpResult1045 + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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
			#define ASE_SRP_VERSION 140009


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
			float4 _FusionAxisColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionTime;
			float _StartCollectionAt;
			float _ExplosionTime;
			float _FusionMulti;
			float _Collection;
			float _ExplosionRadius;
			float _ExplosionRotationSpeed;
			float _MaxFusionExpansion;
			float _Fusion;
			int _UseTime;
			float _FusionJitterFrequency;
			float _FusionJitterFrequencyVariance;
			float _FusionJitterStrength;
			float _FusionRotationStrength;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Transformation;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _FusionPointOnAxisOffset;
			float _FusionPointRingOffsetVariance;
			float _FusionPointRingOffsetStrength;
			float _FusionAxisColor_MinDistance;
			float _FusionAxisColor_MaxDistance;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			real2 ASESafeNormalize(float2 inVec)
			{
				real dp3 = max(FLT_MIN, dot(inVec, inVec));
				return inVec* rsqrt( dp3);
			}
			
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

				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 MovedToFusionPoint264 = _FusionPoint;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g135 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g135 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g135 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g135;
				float temp_output_651_0 = ( ( RandomViaY641 - 0.5 ) * 2.0 );
				float2 appendResult638 = (float2(temp_output_650_0 , temp_output_651_0));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_3 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength * ( 1.0 + ( RandomViaX566 * _FusionPointRingOffsetVariance ) ) );
				float2 temp_cast_4 = (RandomViaZ656).xx;
				float dotResult4_g123 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g123 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g123 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g123 ) , 0.0));
				float4 FusionPositionWithVariance880 = ( float4( MovedToFusionPoint264 , 0.0 ) + appendResult883 );
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float3 rotatedValue785 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - FusionPositionWithVariance880 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedZAroundFusionPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - FusionPositionWithVariance880 );
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Transformation * saturate( ( _Transformation >= temp_output_567_0 ? (0.0 + (( _Transformation - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float4 lerpResult863 = lerp( transform872 , ( FusionPositionWithVariance880 + RotatedYAroundClosesAxisPointOffset790 + RotatedZAroundFusionPointOffset818 ) , Progress_Delayed619);
				float temp_output_963_0 = ( RandomViaZ656 - 0.5 );
				float temp_output_961_0 = ( RandomViaX566 - 0.5 );
				float temp_output_962_0 = ( RandomViaY641 - 0.5 );
				float3 appendResult975 = (float3(temp_output_963_0 , temp_output_961_0 , temp_output_962_0));
				float mulTime970 = _TimeParameters.x * ( _FusionRotationStrength + temp_output_962_0 );
				float3 appendResult953 = (float3(temp_output_961_0 , temp_output_962_0 , temp_output_963_0));
				float3 normalizeResult954 = normalize( appendResult953 );
				float2 temp_cast_9 = (RandomViaY641).xx;
				float dotResult4_g133 = dot( temp_cast_9 , float2( 12.9898,78.233 ) );
				float lerpResult10_g133 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g133 ) * 43758.55 ) ));
				int UseTime545 = _UseTime;
				float mulTime957 = _TimeParameters.x * (float)UseTime545;
				float temp_output_958_0 = sin( ( ( _FusionJitterFrequencyVariance * lerpResult10_g133 ) + ( _FusionJitterFrequency + ( _FusionJitterFrequency * mulTime957 ) ) ) );
				float3 rotatedValue968 = RotateAroundAxis( FusionPoint255, ( FusionPoint255 + ( normalizeResult954 * _FusionJitterStrength * temp_output_958_0 ) ), normalize( appendResult975 ), mulTime970 );
				float3 FusionPointWithNoise1065 = rotatedValue968;
				float temp_output_1062_0 = (_MaxFusionExpansion + (temp_output_958_0 - -1.0) * (1.0 - _MaxFusionExpansion) / (1.0 - -1.0));
				float MoveToFusionPoint1060 = temp_output_1062_0;
				float4 lerpResult899 = lerp( lerpResult863 , float4( FusionPointWithNoise1065 , 0.0 ) , ( _Fusion * MoveToFusionPoint1060 ));
				float3 appendResult997 = (float3(RandomViaZ656 , RandomViaY641 , RandomViaX566));
				float mulTime996 = _TimeParameters.x * _ExplosionRotationSpeed;
				float4 appendResult987 = (float4(( RandomViaX566 - 0.5 ) , ( RandomViaY641 - 0.5 ) , ( RandomViaZ656 - 0.5 ) , 0.0));
				float4 normalizeResult988 = normalize( appendResult987 );
				float CollectionAdjusted219 = _Collection;
				float2 temp_cast_13 = (RandomViaX566).xx;
				float dotResult4_g136 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g136 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g136 ) * 43758.55 ) ));
				float temp_output_1056_0 = ( floor( ( lerpResult10_g136 * _FusionMulti ) ) * _ExplosionTime );
				float3 rotatedValue995 = RotateAroundAxis( FusionPoint255, ( float4( FusionPoint255 , 0.0 ) + ( _ExplosionRadius * normalizeResult988 * sin( ( ( 0.5 * PI ) * saturate( (0.0 + (CollectionAdjusted219 - temp_output_1056_0) * (1.0 - 0.0) / (( temp_output_1056_0 + _ExplosionTime ) - temp_output_1056_0)) ) ) ) ) ).xyz, normalize( appendResult997 ), mulTime996 );
				float3 PositionAfterExplosion999 = rotatedValue995;
				float4 lerpResult982 = lerp( lerpResult899 , float4( PositionAfterExplosion999 , 0.0 ) , ( CollectionAdjusted219 > 0.001 ? 1.0 : 0.0 ));
				float collectionDelay1036 = ( ( ( 1.0 - _StartCollectionAt ) - _CollectionTime ) * RandomViaX566 );
				float MoveToCollectionPoint1043 = saturate( (0.0 + (CollectionAdjusted219 - ( _StartCollectionAt + collectionDelay1036 )) * (1.0 - 0.0) / (( ( _StartCollectionAt + _CollectionTime ) + collectionDelay1036 ) - ( _StartCollectionAt + collectionDelay1036 ))) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , MoveToCollectionPoint1043);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( lerpResult1045 + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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
			#define ASE_SRP_VERSION 140009


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
			float4 _FusionAxisColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionTime;
			float _StartCollectionAt;
			float _ExplosionTime;
			float _FusionMulti;
			float _Collection;
			float _ExplosionRadius;
			float _ExplosionRotationSpeed;
			float _MaxFusionExpansion;
			float _Fusion;
			int _UseTime;
			float _FusionJitterFrequency;
			float _FusionJitterFrequencyVariance;
			float _FusionJitterStrength;
			float _FusionRotationStrength;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Transformation;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _FusionPointOnAxisOffset;
			float _FusionPointRingOffsetVariance;
			float _FusionPointRingOffsetStrength;
			float _FusionAxisColor_MinDistance;
			float _FusionAxisColor_MaxDistance;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			real2 ASESafeNormalize(float2 inVec)
			{
				real dp3 = max(FLT_MIN, dot(inVec, inVec));
				return inVec* rsqrt( dp3);
			}
			
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

				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 MovedToFusionPoint264 = _FusionPoint;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g135 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g135 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g135 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g135;
				float temp_output_651_0 = ( ( RandomViaY641 - 0.5 ) * 2.0 );
				float2 appendResult638 = (float2(temp_output_650_0 , temp_output_651_0));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_3 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength * ( 1.0 + ( RandomViaX566 * _FusionPointRingOffsetVariance ) ) );
				float2 temp_cast_4 = (RandomViaZ656).xx;
				float dotResult4_g123 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g123 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g123 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g123 ) , 0.0));
				float4 FusionPositionWithVariance880 = ( float4( MovedToFusionPoint264 , 0.0 ) + appendResult883 );
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float3 rotatedValue785 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - FusionPositionWithVariance880 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedZAroundFusionPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - FusionPositionWithVariance880 );
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Transformation * saturate( ( _Transformation >= temp_output_567_0 ? (0.0 + (( _Transformation - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float4 lerpResult863 = lerp( transform872 , ( FusionPositionWithVariance880 + RotatedYAroundClosesAxisPointOffset790 + RotatedZAroundFusionPointOffset818 ) , Progress_Delayed619);
				float temp_output_963_0 = ( RandomViaZ656 - 0.5 );
				float temp_output_961_0 = ( RandomViaX566 - 0.5 );
				float temp_output_962_0 = ( RandomViaY641 - 0.5 );
				float3 appendResult975 = (float3(temp_output_963_0 , temp_output_961_0 , temp_output_962_0));
				float mulTime970 = _TimeParameters.x * ( _FusionRotationStrength + temp_output_962_0 );
				float3 appendResult953 = (float3(temp_output_961_0 , temp_output_962_0 , temp_output_963_0));
				float3 normalizeResult954 = normalize( appendResult953 );
				float2 temp_cast_9 = (RandomViaY641).xx;
				float dotResult4_g133 = dot( temp_cast_9 , float2( 12.9898,78.233 ) );
				float lerpResult10_g133 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g133 ) * 43758.55 ) ));
				int UseTime545 = _UseTime;
				float mulTime957 = _TimeParameters.x * (float)UseTime545;
				float temp_output_958_0 = sin( ( ( _FusionJitterFrequencyVariance * lerpResult10_g133 ) + ( _FusionJitterFrequency + ( _FusionJitterFrequency * mulTime957 ) ) ) );
				float3 rotatedValue968 = RotateAroundAxis( FusionPoint255, ( FusionPoint255 + ( normalizeResult954 * _FusionJitterStrength * temp_output_958_0 ) ), normalize( appendResult975 ), mulTime970 );
				float3 FusionPointWithNoise1065 = rotatedValue968;
				float temp_output_1062_0 = (_MaxFusionExpansion + (temp_output_958_0 - -1.0) * (1.0 - _MaxFusionExpansion) / (1.0 - -1.0));
				float MoveToFusionPoint1060 = temp_output_1062_0;
				float4 lerpResult899 = lerp( lerpResult863 , float4( FusionPointWithNoise1065 , 0.0 ) , ( _Fusion * MoveToFusionPoint1060 ));
				float3 appendResult997 = (float3(RandomViaZ656 , RandomViaY641 , RandomViaX566));
				float mulTime996 = _TimeParameters.x * _ExplosionRotationSpeed;
				float4 appendResult987 = (float4(( RandomViaX566 - 0.5 ) , ( RandomViaY641 - 0.5 ) , ( RandomViaZ656 - 0.5 ) , 0.0));
				float4 normalizeResult988 = normalize( appendResult987 );
				float CollectionAdjusted219 = _Collection;
				float2 temp_cast_13 = (RandomViaX566).xx;
				float dotResult4_g136 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g136 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g136 ) * 43758.55 ) ));
				float temp_output_1056_0 = ( floor( ( lerpResult10_g136 * _FusionMulti ) ) * _ExplosionTime );
				float3 rotatedValue995 = RotateAroundAxis( FusionPoint255, ( float4( FusionPoint255 , 0.0 ) + ( _ExplosionRadius * normalizeResult988 * sin( ( ( 0.5 * PI ) * saturate( (0.0 + (CollectionAdjusted219 - temp_output_1056_0) * (1.0 - 0.0) / (( temp_output_1056_0 + _ExplosionTime ) - temp_output_1056_0)) ) ) ) ) ).xyz, normalize( appendResult997 ), mulTime996 );
				float3 PositionAfterExplosion999 = rotatedValue995;
				float4 lerpResult982 = lerp( lerpResult899 , float4( PositionAfterExplosion999 , 0.0 ) , ( CollectionAdjusted219 > 0.001 ? 1.0 : 0.0 ));
				float collectionDelay1036 = ( ( ( 1.0 - _StartCollectionAt ) - _CollectionTime ) * RandomViaX566 );
				float MoveToCollectionPoint1043 = saturate( (0.0 + (CollectionAdjusted219 - ( _StartCollectionAt + collectionDelay1036 )) * (1.0 - 0.0) / (( ( _StartCollectionAt + _CollectionTime ) + collectionDelay1036 ) - ( _StartCollectionAt + collectionDelay1036 ))) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , MoveToCollectionPoint1043);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( lerpResult1045 + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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
			#define ASE_SRP_VERSION 140009


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
			float4 _FusionAxisColor;
			float4 _MainColor;
			float3 _FusionPoint;
			float3 _CollectionPoint;
			float _ParticleScale;
			float _CollectionTime;
			float _StartCollectionAt;
			float _ExplosionTime;
			float _FusionMulti;
			float _Collection;
			float _ExplosionRadius;
			float _ExplosionRotationSpeed;
			float _MaxFusionExpansion;
			float _Fusion;
			int _UseTime;
			float _FusionJitterFrequency;
			float _FusionJitterFrequencyVariance;
			float _FusionJitterStrength;
			float _FusionRotationStrength;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Transformation;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _FusionPointOnAxisOffset;
			float _FusionPointRingOffsetVariance;
			float _FusionPointRingOffsetStrength;
			float _FusionAxisColor_MinDistance;
			float _FusionAxisColor_MaxDistance;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			

			real2 ASESafeNormalize(float2 inVec)
			{
				real dp3 = max(FLT_MIN, dot(inVec, inVec));
				return inVec* rsqrt( dp3);
			}
			
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

				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 MovedToFusionPoint264 = _FusionPoint;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g135 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g135 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g135 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g135;
				float temp_output_651_0 = ( ( RandomViaY641 - 0.5 ) * 2.0 );
				float2 appendResult638 = (float2(temp_output_650_0 , temp_output_651_0));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_3 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength * ( 1.0 + ( RandomViaX566 * _FusionPointRingOffsetVariance ) ) );
				float2 temp_cast_4 = (RandomViaZ656).xx;
				float dotResult4_g123 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g123 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g123 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g123 ) , 0.0));
				float4 FusionPositionWithVariance880 = ( float4( MovedToFusionPoint264 , 0.0 ) + appendResult883 );
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float3 rotatedValue785 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - FusionPositionWithVariance880 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( FusionPoint255, FusionPositionWithVariance880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedZAroundFusionPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - FusionPositionWithVariance880 );
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Transformation * saturate( ( _Transformation >= temp_output_567_0 ? (0.0 + (( _Transformation - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float4 lerpResult863 = lerp( transform872 , ( FusionPositionWithVariance880 + RotatedYAroundClosesAxisPointOffset790 + RotatedZAroundFusionPointOffset818 ) , Progress_Delayed619);
				float temp_output_963_0 = ( RandomViaZ656 - 0.5 );
				float temp_output_961_0 = ( RandomViaX566 - 0.5 );
				float temp_output_962_0 = ( RandomViaY641 - 0.5 );
				float3 appendResult975 = (float3(temp_output_963_0 , temp_output_961_0 , temp_output_962_0));
				float mulTime970 = _TimeParameters.x * ( _FusionRotationStrength + temp_output_962_0 );
				float3 appendResult953 = (float3(temp_output_961_0 , temp_output_962_0 , temp_output_963_0));
				float3 normalizeResult954 = normalize( appendResult953 );
				float2 temp_cast_9 = (RandomViaY641).xx;
				float dotResult4_g133 = dot( temp_cast_9 , float2( 12.9898,78.233 ) );
				float lerpResult10_g133 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g133 ) * 43758.55 ) ));
				int UseTime545 = _UseTime;
				float mulTime957 = _TimeParameters.x * (float)UseTime545;
				float temp_output_958_0 = sin( ( ( _FusionJitterFrequencyVariance * lerpResult10_g133 ) + ( _FusionJitterFrequency + ( _FusionJitterFrequency * mulTime957 ) ) ) );
				float3 rotatedValue968 = RotateAroundAxis( FusionPoint255, ( FusionPoint255 + ( normalizeResult954 * _FusionJitterStrength * temp_output_958_0 ) ), normalize( appendResult975 ), mulTime970 );
				float3 FusionPointWithNoise1065 = rotatedValue968;
				float temp_output_1062_0 = (_MaxFusionExpansion + (temp_output_958_0 - -1.0) * (1.0 - _MaxFusionExpansion) / (1.0 - -1.0));
				float MoveToFusionPoint1060 = temp_output_1062_0;
				float4 lerpResult899 = lerp( lerpResult863 , float4( FusionPointWithNoise1065 , 0.0 ) , ( _Fusion * MoveToFusionPoint1060 ));
				float3 appendResult997 = (float3(RandomViaZ656 , RandomViaY641 , RandomViaX566));
				float mulTime996 = _TimeParameters.x * _ExplosionRotationSpeed;
				float4 appendResult987 = (float4(( RandomViaX566 - 0.5 ) , ( RandomViaY641 - 0.5 ) , ( RandomViaZ656 - 0.5 ) , 0.0));
				float4 normalizeResult988 = normalize( appendResult987 );
				float CollectionAdjusted219 = _Collection;
				float2 temp_cast_13 = (RandomViaX566).xx;
				float dotResult4_g136 = dot( temp_cast_13 , float2( 12.9898,78.233 ) );
				float lerpResult10_g136 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g136 ) * 43758.55 ) ));
				float temp_output_1056_0 = ( floor( ( lerpResult10_g136 * _FusionMulti ) ) * _ExplosionTime );
				float3 rotatedValue995 = RotateAroundAxis( FusionPoint255, ( float4( FusionPoint255 , 0.0 ) + ( _ExplosionRadius * normalizeResult988 * sin( ( ( 0.5 * PI ) * saturate( (0.0 + (CollectionAdjusted219 - temp_output_1056_0) * (1.0 - 0.0) / (( temp_output_1056_0 + _ExplosionTime ) - temp_output_1056_0)) ) ) ) ) ).xyz, normalize( appendResult997 ), mulTime996 );
				float3 PositionAfterExplosion999 = rotatedValue995;
				float4 lerpResult982 = lerp( lerpResult899 , float4( PositionAfterExplosion999 , 0.0 ) , ( CollectionAdjusted219 > 0.001 ? 1.0 : 0.0 ));
				float collectionDelay1036 = ( ( ( 1.0 - _StartCollectionAt ) - _CollectionTime ) * RandomViaX566 );
				float MoveToCollectionPoint1043 = saturate( (0.0 + (CollectionAdjusted219 - ( _StartCollectionAt + collectionDelay1036 )) * (1.0 - 0.0) / (( ( _StartCollectionAt + _CollectionTime ) + collectionDelay1036 ) - ( _StartCollectionAt + collectionDelay1036 ))) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , MoveToCollectionPoint1043);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( lerpResult1045 + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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
Node;AmplifyShaderEditor.CommentaryNode;1044;9219.94,120.1579;Inherit;False;1624.891;824.9747;Comment;16;1034;1035;1033;1037;1036;1043;1032;1042;1040;1041;1028;1039;1038;1020;1014;1018;MoveToCollectionPoint;0.2692986,0.509434,0.2381363,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;977;6744.157,-252.8234;Inherit;False;2316.881;1259.911;Comment;31;968;976;971;900;961;953;975;964;957;955;966;967;952;903;951;959;958;905;963;962;954;970;973;904;979;980;1060;1062;1065;1066;1067;Fusion;0,0.7830189,0.6391835,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;898;6193.002,-2201.317;Inherit;False;929.7021;992.0812;Comment;4;838;837;835;896;TransformationState;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;875;6625.096,-1121.581;Inherit;False;763.7163;585.0416;Add some jiggle to it;1;872;SpawnState;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;661;3258.221,-4387.824;Inherit;False;1515.317;661.3047;Comment;19;644;652;650;651;638;636;653;643;645;635;659;660;657;632;884;888;889;890;893;OffsetFromFusionAxis;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;616;1170.652,-6725.051;Inherit;False;2195.834;649.1808;Comment;13;615;614;597;601;605;606;599;604;623;627;617;626;629;MoveToFusionAxis;0.5263038,0.5465524,0.9528302,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;590;10152.07,-1550.257;Inherit;False;742.2939;484.9873;vertices;6;373;371;372;374;562;563;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;565;4171.773,-359.4157;Inherit;False;1983.349;986.3488;Comment;21;856;857;852;855;854;526;515;531;525;533;529;538;540;539;519;537;532;516;858;859;861;CenterColor;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;459;1110.046,-2039.087;Inherit;False;2744.508;923.9855;Comment;32;2;3;4;5;6;7;8;9;440;434;333;449;330;436;439;435;458;452;129;447;455;441;438;426;450;566;567;619;622;847;848;849;Delay;0.6597723,0.2776922,0.8773585,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;265;1990.444,-3554.395;Inherit;False;1103.841;648.4066;;10;194;255;215;264;631;620;840;841;844;842;Move To Fusion Point;0.6653691,0.8490566,0.6127626,1;0;0
Node;AmplifyShaderEditor.StickyNoteNode;440;1907.9,-1989.087;Inherit;False;228.8146;130.6501;;;1,1,1,1;Make sure that delay + catchup is within one, otherwise some particles might not catch up before the end.$;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;436;1652.258,-1868.701;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DistanceOpNode;516;4468.436,-179.3884;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;532;4531.541,163.2056;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;537;4264.074,-83.41417;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;519;4221.773,64.34956;Inherit;False;Property;_FusionColorRange;FusionColorRange;16;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;539;4808.566,-89.84628;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;540;5118.568,65.15376;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;538;4988.566,-156.8464;Inherit;False;2;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;533;4932.485,67.09077;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;525;4343.246,290.7799;Inherit;False;Property;_FusionColorRangeFallOff;FusionColorRangeFallOff;17;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;531;4722.776,287.1691;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;638;3915.298,-4198.826;Inherit;False;FLOAT2;4;0;FLOAT;0.6;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;659;3957.533,-3896.392;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;663;3749.409,-7470.825;Inherit;False;1864.76;1237.109;Rotation;19;665;668;670;688;669;687;666;675;673;683;671;676;674;672;772;773;774;775;680;Rotate Around Fusion Axis Point X;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;10368.8,-2693.637;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;491;-595.591,-3604.407;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;494;-127.5913,-3430.407;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.NormalizeNode;495;54.40864,-3395.407;Inherit;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;497;149.9587,-3285.42;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;499;467.6057,-3368.432;Inherit;False;FLOAT3;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;496;279.5246,-3423.939;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;492;-321.5912,-3519.407;Inherit;False;Random Range;-1;;111;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;-1;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;503;-22.57719,-3277.163;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;493;-320.5912,-3369.407;Inherit;False;Random Range;-1;;112;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;-1;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;504;-316.5771,-3218.163;Inherit;False;Random Range;-1;;113;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;490;-321.5751,-3080.933;Inherit;False;Property;_OffsetFromFusionPointVariation;OffsetFromFusionPointVariation;19;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;489;-66.42014,-3612.85;Inherit;False;Property;_OffsetFromFusionPointStrength;OffsetFromFusionPointStrength;18;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;777;6001.128,-4202.071;Inherit;False;1864.76;1237.109;Rotation;19;797;796;795;793;792;791;790;788;787;786;785;784;783;782;781;780;779;778;794;Rotate Around Fusion Axis Point X;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.SimpleTimeNode;778;6437.876,-3628.244;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;779;6998.144,-4045.06;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;780;6224.022,-3626.344;Inherit;False;545;UseTime;1;0;OBJECT;;False;1;INT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;782;6797.233,-3662.184;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;783;6722.523,-4007.062;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;784;7112.507,-3226.274;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;785;6719.475,-3411.801;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RadiansOpNode;791;7216.116,-3917.969;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;792;6433.028,-3848.774;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;793;6578.589,-3982.763;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;795;6223.085,-3910.684;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;796;6010.458,-3823.208;Inherit;False;641;RandomViaY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;794;6268.696,-3829.847;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;781;6444.087,-4146.144;Inherit;False;Property;_RotateY_Speed;RotateY_Speed;12;0;Create;True;0;0;0;False;0;False;0;249;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;797;6023.354,-3741.79;Inherit;False;Property;_RotateY_Variance;RotateY_Variance;13;0;Create;True;0;0;0;False;0;False;10;0.05;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;799;7881.625,-2858.828;Inherit;False;1864.76;1237.109;Rotation;21;818;817;816;815;813;812;811;810;809;808;807;806;805;804;803;801;800;839;878;877;894;Rotate Around Fusion Axis Point Z;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.SimpleTimeNode;800;8318.375,-2285.001;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;801;8878.643,-2701.817;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;803;8677.732,-2318.941;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;804;8603.021,-2663.819;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;805;8993.006,-1883.03;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RadiansOpNode;807;9096.615,-2574.726;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;808;8313.527,-2505.531;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;809;8459.088,-2639.52;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;810;8103.583,-2567.441;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;811;7890.956,-2479.965;Inherit;False;641;RandomViaY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;371;10420.07,-1370.207;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;563;10762.45,-1266.659;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.PosVertexDataNode;372;10174.36,-1497.608;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TransformPositionNode;373;10570.11,-1514.834;Inherit;True;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;374;10176.33,-1240.373;Inherit;False;Property;_ParticleScale;ParticleScale;3;0;Create;True;0;0;0;False;0;False;0.3;0.057;0;0.3;0;1;FLOAT;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;806;8741.036,-2111.354;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;812;7897.837,-2703.874;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;813;8324.586,-2802.901;Inherit;False;Property;_RotateZ_Speed;RotateZ_Speed;14;0;Create;True;0;0;0;False;0;False;0;229;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;817;7903.852,-2398.547;Inherit;False;Property;_RotateZ_Variance;RotateZ_Variance;15;0;Create;True;0;0;0;False;0;False;10;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;816;8512.547,-2176.927;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;845;2191.866,-2868.021;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;8;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;9;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;449;1529.725,-1971.735;Inherit;False;DelayCatchUp;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;439;1711.248,-1577.098;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;458;1498.921,-1492.87;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;435;1872.905,-1746.011;Inherit;False;2;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;426;2245.976,-1731.339;Inherit;False;Random Range;-1;;117;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;847;2037.215,-1232.458;Inherit;False;Property;_ProgressDelay_Min;ProgressDelay_Min;2;0;Create;True;0;0;0;False;0;False;0;0.115;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;452;3374.843,-1740.409;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;441;3196.109,-1764.014;Inherit;False;3;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;447;3068.555,-1564.195;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;455;2875.164,-1711.886;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;567;2752.004,-1502.432;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;849;2625.571,-1356.349;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;848;2397.317,-1301.306;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;438;1952.007,-1404.678;Inherit;False;DelayCapped;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;529;5289.352,136.1183;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;852;5299.484,-78.13897;Inherit;False;Property;_FusionAxisColor_MinDistance;FusionAxisColor_MinDistance;6;0;Create;True;0;0;0;False;0;False;0;0.41;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;854;5296.541,-0.2754244;Inherit;False;Property;_FusionAxisColor_MaxDistance;FusionAxisColor_MaxDistance;7;0;Create;True;0;0;0;False;0;False;0;1.04;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;859;5872.437,-93.28534;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;857;5846.007,-311.7293;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;526;5829.425,157.0957;Inherit;False;FusionColorAmount;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;856;5127.069,-207.4219;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TFHCRemapNode;858;5672.638,-78.12498;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;850;5120.002,-456.617;Inherit;False;615;ClosestPointOnFusionAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DistanceOpNode;855;5443.452,-273.5324;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DistanceOpNode;860;5451.848,-429.3688;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;861;5650.313,-341.7578;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;515;5097.616,-343.7786;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;498;618.8747,-3334.052;Inherit;False;OffsetAroundFusionPoint;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;215;1988.355,-3510.395;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector3Node;194;1988.914,-3320.544;Inherit;False;Property;_FusionPoint;FusionPoint;24;0;Create;False;0;0;0;False;0;False;0,0,0;0.007405923,1.85188,1.126751;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;842;2691.083,-3132.095;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;844;2903.437,-2953.656;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;841;2626.786,-2997.848;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;846;2519.584,-2870.184;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;840;2049.539,-2985.789;Inherit;False;Constant;_mo;mo;28;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;631;2567.305,-3471.099;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;562;10522.08,-1228.122;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;622;3476.688,-1884.409;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;788;6357.445,-3491.246;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;790;7300.373,-3468.726;Inherit;False;RotatedYAroundClosesAxisPointOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;815;7941.816,-2103.994;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;660;4199.431,-3866.092;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;620;2365.95,-3511.365;Inherit;False;619;Progress Delayed;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;255;2323.714,-3181.294;Inherit;False;FusionPoint;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.NormalizeNode;644;4157.891,-4164.843;Inherit;False;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;639;-582.4109,-1634.699;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;635;4002.456,-4031.538;Inherit;False;Property;_FusionPointRingOffsetStrength;FusionPointRingOffsetStrength;21;0;Create;True;0;0;0;False;0;False;0.0434969;0.405;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;879;4758.555,-3983.913;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;632;4330.53,-3789.605;Inherit;False;264;MovedToFusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;645;4375.654,-4015.964;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.BreakToComponentsNode;884;4505.154,-4188.953;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.DynamicAppendNode;883;4707.241,-4179.629;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;652;3567.593,-4279.751;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;650;3772.031,-4312.524;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;636;3315.492,-4337.569;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;881;3787.144,-4590.234;Inherit;False;Property;_FusionPointOnAxisOffset;FusionPointOnAxisOffset;22;0;Create;True;0;0;0;False;0;False;0.0434969;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;657;3531.824,-3802.597;Inherit;False;Property;_FusionPointRingOffsetVariance;FusionPointRingOffsetVariance;23;0;Create;True;0;0;0;False;0;False;0;0;0;2.17;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;643;3285.201,-4195.503;Inherit;False;641;RandomViaY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;889;3983.278,-4380.729;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;888;4349.928,-4317.804;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;890;4126.674,-4379.001;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;653;3532.23,-4130.217;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;651;3712.594,-4149.307;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;658;3058.78,-3921.689;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;818;9179.871,-2124;Inherit;False;RotatedZAroundFusionPointOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.StickyNoteNode;894;8582.505,-1934.203;Inherit;False;332.6025;127.3905;New Note;;1,1,1,1;The idea here is to only calculate offset, so it's much easier to add them together. If we take the final position and add various rotations, it will keep adding the offset from the spawnpoint$The alternative would be to rotate the entire position one after another, pluggin in the result of one rotation into the next one.;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;786;6258.84,-3172.461;Inherit;False;880;FusionPositionWithVariance;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;787;6278.381,-3326.343;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;880;4965.611,-3941.175;Inherit;False;FusionPositionWithVariance;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;839;8163.858,-1788.351;Inherit;False;880;FusionPositionWithVariance;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;877;8124.275,-1971.632;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;878;8234.653,-2147.604;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;129;2203.838,-1980.186;Inherit;False;Property;_Transformation;Transformation;1;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;604;1692.677,-6449.249;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;606;1859.702,-6334.335;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;627;2747.768,-6158.316;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;623;2470.315,-6098.052;Inherit;False;619;Progress Delayed;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;599;1493.501,-6508.839;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;601;1243.425,-6432.498;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;597;1474.569,-6298.794;Inherit;False;Property;_FusionDirection;FusionDirection;25;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;615;2297.098,-6493.957;Inherit;False;ClosestPointOnFusionAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleTimeNode;672;4186.157,-6896.999;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;674;4746.424,-7313.814;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;683;4192.368,-7414.898;Inherit;False;Property;_RotateX_Speed;RotateX_Speed;10;0;Create;True;0;0;0;False;0;False;0;0;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;673;4545.514,-6930.938;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;675;4470.804,-7275.816;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;687;4860.788,-6495.028;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;669;4467.755,-6680.555;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;688;4007.121,-6441.215;Inherit;False;880;FusionPositionWithVariance;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;668;4253.726,-6744;Inherit;False;FLOAT3;4;0;FLOAT;1;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RadiansOpNode;666;4964.397,-7186.723;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;775;4181.309,-7117.528;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;671;4326.87,-7251.518;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;772;3749.317,-7203.659;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;774;3971.366,-7179.438;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;773;3758.738,-7091.962;Inherit;False;641;RandomViaY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;680;3771.635,-7008.538;Inherit;False;Property;_RotateX_Variance;RotateX_Variance;11;0;Create;True;0;0;0;False;0;False;10;1.2;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;676;3893.303,-6864.098;Inherit;False;545;UseTime;1;0;OBJECT;;False;1;INT;0
Node;AmplifyShaderEditor.GetLocalVarNode;670;3821.301,-6672.097;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;617;2093.763,-6199.351;Inherit;False;Property;_MoveToFusionAxisEarly;MoveToFusionAxisEarly;20;0;Create;True;0;0;0;False;0;False;0;0;0;0.99;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;626;2600.033,-6184.19;Inherit;False;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;625;2389.972,-6110.229;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;629;2813.734,-6209.393;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;665;5088.636,-6760.083;Inherit;False;RotatedXAroundClosesAxisPointOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;434;1160.88,-1926.267;Inherit;False;Property;_DelayCatchUpTime;DelayCatchUpTime;9;0;Create;True;0;0;0;False;0;False;1;0.021;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;333;2000.433,-1824.635;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;450;2863.167,-1370.804;Inherit;False;449;DelayCatchUp;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;330;1176.429,-1764.053;Inherit;False;Property;_ProgressDelay_Variance;ProgressDelay_Variance;8;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;619;3658.772,-1885.414;Inherit;False;Progress Delayed;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;838;6976.705,-2151.317;Inherit;False;3;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;896;6423.281,-2155.463;Inherit;False;880;FusionPositionWithVariance;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;837;6453.528,-1914.971;Inherit;False;818;RotatedZAroundFusionPointOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;835;6415.597,-2033.836;Inherit;False;790;RotatedYAroundClosesAxisPointOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;872;6662.434,-1027.77;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WireNode;866;9280.163,-1405.773;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.NormalizeNode;954;7673.777,361.2044;Inherit;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;963;7291.528,476.6742;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;976;7744.33,81.47413;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;970;7858.137,-130.3189;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;541;-336.4528,-2318.454;Inherit;False;Property;_UseTime;UseTime;0;0;Create;True;0;0;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;545;-42.61488,-2281.764;Inherit;False;UseTime;-1;True;1;0;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.FunctionNode;973;7123.431,728.2463;Inherit;False;Random Range;-1;;133;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;980;7573.389,859.1777;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;978;7118.52,977.5683;Inherit;False;545;UseTime;1;0;OBJECT;;False;1;INT;0
Node;AmplifyShaderEditor.SimpleTimeNode;957;7390.113,974.6697;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;523;8608.611,-1505.974;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;10;7779.413,-1589.841;Inherit;False;Property;_MainColor;MainColor;4;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,0.9284637,6.115068,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;518;7780.057,-1325.04;Inherit;False;Property;_FusionAxisColor;FusionAxisColor;5;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,3.721394,16,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;527;8037.358,-1258.302;Inherit;False;526;FusionColorAmount;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;903;6818.81,94.6782;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;961;7043.533,137.117;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;953;7465.372,189.2624;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;955;8117.289,520.7686;Inherit;False;3;3;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;959;8339.755,480.3208;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;264;2763.369,-3264.517;Inherit;False;MovedToFusionPoint;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;987;10449.09,-516.1365;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.NormalizeNode;988;10594.93,-428.1588;Inherit;False;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;992;10275.04,-579.7675;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;993;10272.65,-451.623;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;984;10028.2,-639.554;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;983;10705.3,-229.4646;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;991;10826.23,-693.21;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;990;10729.03,-588.4022;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleTimeNode;996;10179.86,-885.135;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;997;10469.95,-911.1;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PiNode;1010;9622.031,-946.769;Inherit;False;1;0;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;1012;10026.53,-777.894;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;1006;9548.923,-700.0807;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;994;10424.93,-346.8731;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1018;9269.94,314.2152;Inherit;False;Property;_CollectionTime;CollectionTime;39;0;Create;True;0;0;0;False;0;False;0.044;0;0;0.5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1014;9285.646,206.9208;Inherit;False;Property;_StartCollectionAt;StartCollectionAt;38;0;Create;True;0;0;0;False;0;False;0;0;0;0.99;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;1020;10149.75,718.3566;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1038;9641.679,649.7886;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1039;9843.792,783.3409;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1028;9951.192,577.7051;Inherit;False;219;CollectionAdjusted;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1041;10020.72,660.8969;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1040;9618.49,832.3186;Inherit;False;1036;collectionDelay;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1042;9792.976,698.8546;Inherit;False;1036;collectionDelay;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;1032;10352.37,672.2625;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1036;10397.58,264.7596;Inherit;False;collectionDelay;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1037;10144.21,356.0493;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1033;9868.267,440.0725;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;1035;9929.385,296.1257;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;1034;9689.55,216.9215;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;9903.502,-1575.562;Float;False;True;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;Potato 3 Axis Rotation Fusion;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;1;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;;0;0;Standard;23;Surface;0;0;  Blend;0;0;Two Sided;1;0;Forward Only;0;0;Cast Shadows;0;638332276799351495;  Use Shadow Threshold;0;0;Receive Shadows;0;638332276827847575;GPU Instancing;1;638332271287541535;LOD CrossFade;0;0;Built-in Fog;0;0;DOTS Instancing;0;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Vertex Position,InvertActionOnDeselection;0;638274534278721169;0;10;False;True;False;True;False;False;True;True;True;False;False;;False;0
Node;AmplifyShaderEditor.TransformPositionNode;26;9675.874,-1421.787;Inherit;False;World;Object;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;589;9539.984,-1323.049;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LerpOp;982;8929.921,-1234.942;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;995;11027.85,-827.1302;Inherit;False;True;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;998;9931.065,-977.126;Inherit;False;Property;_ExplosionRotationSpeed;ExplosionRotationSpeed;31;0;Create;True;0;0;0;False;0;False;1;1;-2.02;3.54;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;218;-324.4799,-1958.653;Inherit;False;Property;_Collection;Collection;36;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;219;28.26842,-1929.622;Inherit;False;CollectionAdjusted;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;1045;9342.85,-1266.841;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;989;10434.16,-743.9446;Inherit;False;Property;_ExplosionRadius;ExplosionRadius;30;0;Create;True;0;0;0;False;0;False;0;1;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1009;9871.142,-848.7565;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;1008;9714.223,-777.4588;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;654;-316.3819,-1456.945;Inherit;False;Random Range;-1;;134;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1007;9258.896,-803.1445;Inherit;False;219;CollectionAdjusted;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;986;10013.25,-445.8044;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;566;2541.962,-1686.032;Inherit;False;RandomViaX;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;656;-100.1951,-1498.964;Inherit;False;RandomViaZ;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;641;-98.28695,-1590.338;Inherit;False;RandomViaY;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FloorOpNode;1051;9086.528,-625.8625;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1054;8907.729,-596.7624;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1056;9247.845,-676.1348;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1055;9403.158,-580.3543;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;981;8969.347,-482.313;Inherit;False;Property;_ExplosionTime;ExplosionTime;26;0;Create;True;0;0;0;False;0;False;0;1;0;0.5;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1053;8549.713,-831.9333;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;649;-327.8249,-1610.539;Inherit;False;Random Range;-1;;135;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;985;9974.07,-531.1841;Inherit;False;641;RandomViaY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;1058;8770.339,-754.2263;Inherit;False;Random Range;-1;;136;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;1013;9113.433,-1164.685;Inherit;False;Property;_CollectionPoint;CollectionPoint;37;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.LerpOp;863;7857.694,-907.4036;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LerpOp;899;8216.146,-1130.731;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1043;10522.96,636.6535;Inherit;False;MoveToCollectionPoint;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1001;8469.502,-1006.041;Inherit;False;219;CollectionAdjusted;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1000;8485.315,-1091.626;Inherit;False;999;PositionAfterExplosion;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;967;7888.223,621.3209;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;952;6865.328,342.5274;Inherit;False;641;RandomViaY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;951;6787.898,542.5615;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;962;7190.991,329.1033;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;975;8007.849,161.449;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;905;7492.112,482.7006;Inherit;False;Property;_FusionJitterStrength;FusionJitterStrength;32;0;Create;True;0;0;0;False;0;False;0;3.42;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;904;7296.329,805.068;Inherit;False;Property;_FusionJitterFrequency;FusionJitterFrequency;34;0;Create;True;0;0;0;False;0;False;0;1.49;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;964;7113.423,619.074;Inherit;False;Property;_FusionJitterFrequencyVariance;FusionJitterFrequencyVariance;33;0;Create;True;0;0;0;False;0;False;0;1.12;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;966;7483.316,642.4803;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;979;7724.739,680.5829;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;958;8023.739,738.7021;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;1062;8214.431,693.0839;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;-1;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;876;7575.173,-782.0571;Inherit;False;619;Progress Delayed;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;971;7390.043,-120.8264;Inherit;False;Property;_FusionRotationStrength;FusionRotationStrength;35;0;Create;True;0;0;0;False;0;False;0;0.88;0;3;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1063;7740.93,-686.805;Inherit;False;1060;MoveToFusionPoint;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1064;8107.37,-764.6493;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;968;8516.905,365.0827;Inherit;False;True;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;900;7900.057,341.2578;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1065;8584.254,44.84045;Inherit;False;FusionPointWithNoise;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;1047;8584.245,-559.6453;Inherit;False;Property;_FusionMulti;FusionMulti;28;0;Create;True;0;0;0;False;0;False;1;0.5;1;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1060;8676.061,705.6185;Inherit;False;MoveToFusionPoint;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;1067;8477.496,735.4486;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1066;7978.716,917.5263;Inherit;False;Property;_MaxFusionExpansion;MaxFusionExpansion;27;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;897;7811.059,-568.9843;Inherit;False;Property;_Fusion;Fusion;29;0;Create;True;0;0;0;False;0;False;0;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1046;9091.522,-995.3547;Inherit;False;1043;MoveToCollectionPoint;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;999;11385.54,-855.6627;Inherit;False;PositionAfterExplosion;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Compare;1005;8762.64,-1020.285;Inherit;False;2;4;0;FLOAT;0;False;1;FLOAT;0.001;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;605;2084.54,-6388.21;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;614;1933.122,-6642.842;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;893;3679.943,-4488.379;Inherit;False;Random Range;-1;;123;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;-1;False;3;FLOAT;1;False;1;FLOAT;0
WireConnection;436;0;434;0
WireConnection;436;1;330;0
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
WireConnection;533;0;516;0
WireConnection;533;1;519;0
WireConnection;533;3;531;0
WireConnection;531;0;532;0
WireConnection;531;2;525;0
WireConnection;638;0;650;0
WireConnection;638;1;651;0
WireConnection;659;0;658;0
WireConnection;659;1;657;0
WireConnection;494;0;492;0
WireConnection;494;1;493;0
WireConnection;495;0;494;0
WireConnection;497;1;503;0
WireConnection;499;0;496;0
WireConnection;496;0;489;0
WireConnection;496;1;497;0
WireConnection;496;2;495;0
WireConnection;492;1;491;1
WireConnection;503;0;504;0
WireConnection;503;1;490;0
WireConnection;493;1;491;2
WireConnection;504;1;491;3
WireConnection;779;0;783;0
WireConnection;779;1;782;0
WireConnection;782;0;783;0
WireConnection;782;1;778;0
WireConnection;783;0;781;0
WireConnection;783;1;793;0
WireConnection;784;0;785;0
WireConnection;784;1;786;0
WireConnection;785;0;788;0
WireConnection;785;1;791;0
WireConnection;785;2;787;0
WireConnection;785;3;786;0
WireConnection;791;0;779;0
WireConnection;792;0;795;0
WireConnection;792;1;797;0
WireConnection;793;1;792;0
WireConnection;795;0;794;0
WireConnection;795;1;796;0
WireConnection;801;0;804;0
WireConnection;801;1;803;0
WireConnection;803;0;804;0
WireConnection;803;1;800;0
WireConnection;804;0;813;0
WireConnection;804;1;809;0
WireConnection;805;0;806;0
WireConnection;805;1;839;0
WireConnection;807;0;801;0
WireConnection;808;0;810;0
WireConnection;808;1;817;0
WireConnection;809;1;808;0
WireConnection;810;0;812;0
WireConnection;810;1;811;0
WireConnection;371;0;372;0
WireConnection;371;1;374;0
WireConnection;563;0;373;0
WireConnection;563;1;562;0
WireConnection;373;0;371;0
WireConnection;806;0;816;0
WireConnection;806;1;807;0
WireConnection;806;2;815;0
WireConnection;806;3;839;0
WireConnection;449;0;434;0
WireConnection;439;0;330;0
WireConnection;439;1;458;0
WireConnection;458;0;436;0
WireConnection;435;0;436;0
WireConnection;435;2;439;0
WireConnection;435;3;330;0
WireConnection;426;1;333;1
WireConnection;452;0;441;0
WireConnection;441;0;129;0
WireConnection;441;1;567;0
WireConnection;441;2;447;0
WireConnection;447;0;455;0
WireConnection;447;2;450;0
WireConnection;455;0;129;0
WireConnection;455;1;567;0
WireConnection;567;0;849;0
WireConnection;567;1;438;0
WireConnection;849;0;848;0
WireConnection;848;0;566;0
WireConnection;848;3;847;0
WireConnection;438;0;435;0
WireConnection;529;0;540;0
WireConnection;859;0;858;0
WireConnection;857;0;861;0
WireConnection;857;1;852;0
WireConnection;526;0;859;0
WireConnection;858;0;861;0
WireConnection;858;1;852;0
WireConnection;858;2;854;0
WireConnection;855;0;515;0
WireConnection;855;1;856;0
WireConnection;860;0;850;0
WireConnection;860;1;856;0
WireConnection;861;0;860;0
WireConnection;861;1;855;0
WireConnection;498;0;499;0
WireConnection;842;0;620;0
WireConnection;842;1;841;0
WireConnection;844;0;842;0
WireConnection;841;1;846;0
WireConnection;846;0;840;0
WireConnection;846;1;845;0
WireConnection;631;0;215;0
WireConnection;631;1;194;0
WireConnection;631;2;620;0
WireConnection;622;0;129;0
WireConnection;622;1;452;0
WireConnection;790;0;784;0
WireConnection;660;1;659;0
WireConnection;255;0;194;0
WireConnection;644;0;638;0
WireConnection;879;0;632;0
WireConnection;879;1;883;0
WireConnection;645;0;644;0
WireConnection;645;1;635;0
WireConnection;645;2;660;0
WireConnection;884;0;645;0
WireConnection;883;0;884;0
WireConnection;883;1;884;1
WireConnection;883;2;888;0
WireConnection;652;0;636;0
WireConnection;650;0;652;0
WireConnection;889;0;650;0
WireConnection;889;1;651;0
WireConnection;888;0;881;0
WireConnection;888;1;893;0
WireConnection;890;0;889;0
WireConnection;653;0;643;0
WireConnection;651;0;653;0
WireConnection;818;0;805;0
WireConnection;880;0;879;0
WireConnection;604;0;599;0
WireConnection;604;1;597;0
WireConnection;606;0;604;0
WireConnection;606;1;597;0
WireConnection;627;0;623;0
WireConnection;627;1;626;0
WireConnection;599;0;669;0
WireConnection;599;1;601;0
WireConnection;615;0;614;0
WireConnection;674;0;675;0
WireConnection;674;1;673;0
WireConnection;673;0;675;0
WireConnection;673;1;672;0
WireConnection;675;0;683;0
WireConnection;675;1;671;0
WireConnection;687;0;669;0
WireConnection;687;1;688;0
WireConnection;669;0;668;0
WireConnection;669;1;666;0
WireConnection;669;2;670;0
WireConnection;669;3;688;0
WireConnection;666;0;674;0
WireConnection;775;0;774;0
WireConnection;775;1;680;0
WireConnection;671;1;775;0
WireConnection;774;0;772;0
WireConnection;774;1;773;0
WireConnection;626;1;625;0
WireConnection;625;0;617;0
WireConnection;629;0;627;0
WireConnection;665;0;687;0
WireConnection;619;0;622;0
WireConnection;838;0;896;0
WireConnection;838;1;835;0
WireConnection;838;2;837;0
WireConnection;866;0;523;0
WireConnection;954;0;953;0
WireConnection;963;0;951;0
WireConnection;976;0;971;0
WireConnection;976;1;962;0
WireConnection;970;0;976;0
WireConnection;545;0;541;0
WireConnection;973;1;952;0
WireConnection;980;0;904;0
WireConnection;980;1;957;0
WireConnection;957;0;978;0
WireConnection;523;0;10;0
WireConnection;523;1;518;0
WireConnection;523;2;527;0
WireConnection;961;0;903;0
WireConnection;953;0;961;0
WireConnection;953;1;962;0
WireConnection;953;2;963;0
WireConnection;955;0;954;0
WireConnection;955;1;905;0
WireConnection;955;2;958;0
WireConnection;959;0;900;0
WireConnection;959;1;955;0
WireConnection;264;0;194;0
WireConnection;987;0;992;0
WireConnection;987;1;993;0
WireConnection;987;2;994;0
WireConnection;988;0;987;0
WireConnection;992;0;984;0
WireConnection;993;0;985;0
WireConnection;991;0;983;0
WireConnection;991;1;990;0
WireConnection;990;0;989;0
WireConnection;990;1;988;0
WireConnection;990;2;1012;0
WireConnection;996;0;998;0
WireConnection;997;0;986;0
WireConnection;997;1;985;0
WireConnection;997;2;984;0
WireConnection;1012;0;1009;0
WireConnection;1006;0;1007;0
WireConnection;1006;1;1056;0
WireConnection;1006;2;1055;0
WireConnection;994;0;986;0
WireConnection;1020;0;1028;0
WireConnection;1020;1;1041;0
WireConnection;1020;2;1039;0
WireConnection;1038;0;1014;0
WireConnection;1038;1;1018;0
WireConnection;1039;0;1038;0
WireConnection;1039;1;1040;0
WireConnection;1041;0;1014;0
WireConnection;1041;1;1042;0
WireConnection;1032;0;1020;0
WireConnection;1036;0;1037;0
WireConnection;1037;0;1035;0
WireConnection;1037;1;1033;0
WireConnection;1035;0;1034;0
WireConnection;1035;1;1018;0
WireConnection;1034;0;1014;0
WireConnection;1;2;866;0
WireConnection;1;5;26;0
WireConnection;26;0;589;0
WireConnection;589;0;1045;0
WireConnection;589;1;563;0
WireConnection;982;0;899;0
WireConnection;982;1;1000;0
WireConnection;982;2;1005;0
WireConnection;995;0;997;0
WireConnection;995;1;996;0
WireConnection;995;2;983;0
WireConnection;995;3;991;0
WireConnection;219;0;218;0
WireConnection;1045;0;982;0
WireConnection;1045;1;1013;0
WireConnection;1045;2;1046;0
WireConnection;1009;0;1010;0
WireConnection;1009;1;1008;0
WireConnection;1008;0;1006;0
WireConnection;654;1;639;3
WireConnection;566;0;426;0
WireConnection;656;0;654;0
WireConnection;641;0;649;0
WireConnection;1051;0;1054;0
WireConnection;1054;0;1058;0
WireConnection;1054;1;1047;0
WireConnection;1056;0;1051;0
WireConnection;1056;1;981;0
WireConnection;1055;0;1056;0
WireConnection;1055;1;981;0
WireConnection;649;1;639;2
WireConnection;1058;1;1053;0
WireConnection;863;0;872;0
WireConnection;863;1;838;0
WireConnection;863;2;876;0
WireConnection;899;0;863;0
WireConnection;899;1;1065;0
WireConnection;899;2;1064;0
WireConnection;1043;0;1032;0
WireConnection;967;0;966;0
WireConnection;967;1;979;0
WireConnection;962;0;952;0
WireConnection;975;0;963;0
WireConnection;975;1;961;0
WireConnection;975;2;962;0
WireConnection;966;0;964;0
WireConnection;966;1;973;0
WireConnection;979;0;904;0
WireConnection;979;1;980;0
WireConnection;958;0;967;0
WireConnection;1062;0;958;0
WireConnection;1062;3;1066;0
WireConnection;1064;0;897;0
WireConnection;1064;1;1063;0
WireConnection;968;0;975;0
WireConnection;968;1;970;0
WireConnection;968;2;900;0
WireConnection;968;3;959;0
WireConnection;1065;0;968;0
WireConnection;1060;0;1062;0
WireConnection;1067;0;1062;0
WireConnection;1067;1;1066;0
WireConnection;999;0;995;0
WireConnection;1005;0;1001;0
WireConnection;605;0;614;0
WireConnection;605;1;606;0
WireConnection;893;1;636;0
ASEEND*/
//CHKSM=0436462E3244D2BE59BE9E38A0119EA827FF9DD5