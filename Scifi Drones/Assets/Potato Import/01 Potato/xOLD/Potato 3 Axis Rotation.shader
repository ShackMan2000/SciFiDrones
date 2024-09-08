// Made with Amplify Shader Editor v1.9.2.1
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Potato 3 Axis Rotation"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_Progress("Progress", Range( 0 , 1)) = 0
		_ProgressDelay_Min("ProgressDelay_Min", Range( 0 , 1)) = 0
		_CollectionStagger("CollectionStagger", Range( 0 , 1)) = 0
		_ParticleScale("ParticleScale", Range( 0 , 0.3)) = 0.3
		[HDR]_MainColor("MainColor", Color) = (0.2042542,0.5284038,0.9622642,0)
		[HDR]_FusionAxisColor("FusionAxisColor", Color) = (0.2042542,0.5284038,0.9622642,0)
		_FusionAxisColor_MinDistance("FusionAxisColor_MinDistance", Float) = 0
		_FusionAxisColor_MaxDistance("FusionAxisColor_MaxDistance", Float) = 0
		_ProgressDelay_Variance("ProgressDelay_Variance", Range( 0 , 1)) = 1
		_DelayCatchUpTime("DelayCatchUpTime", Range( 0 , 1)) = 1
		_RotateX_ProgressRange("RotateX_ProgressRange", Vector) = (0.2,0.3,1,0)
		_RotateX_Speed("RotateX_Speed", Range( 0 , 500)) = 0
		_RotateX_Variance("RotateX_Variance", Range( 0 , 10)) = 10
		_RotateY_ProgressRange("RotateY_ProgressRange", Vector) = (0.2,0.3,1,0)
		_RotateY_Speed("RotateY_Speed", Range( 0 , 500)) = 0
		_RotateY_Variance("RotateY_Variance", Range( 0 , 10)) = 10
		_RotateZ_ProgressRange("RotateZ_ProgressRange", Vector) = (0.2,0.3,1,0)
		_RotateZ_Speed("RotateZ_Speed", Range( 0 , 500)) = 0
		_RotateZ_Variance("RotateZ_Variance", Range( 0 , 10)) = 10
		_MoveToFusionAxisEarly("MoveToFusionAxisEarly", Range( 0 , 0.99)) = 0
		_OffsetFusionAxisStrength("OffsetFusionAxisStrength", Range( 0 , 1)) = 0
		_OffsetFusionAxisVariance("OffsetFusionAxisVariance", Range( 0 , 2.17)) = 0
		_MoveToFusionPoint_Variance("MoveToFusionPoint_Variance", Range( 0 , 3)) = 0
		_CollectionPoint("CollectionPoint", Vector) = (0,0,0,0)
		_FusionPoint("FusionPoint", Vector) = (0,0,0,0)
		_FusionDirection("FusionDirection", Vector) = (0,0,0,0)


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
			float3 _RotateZ_ProgressRange;
			float3 _FusionDirection;
			float3 _RotateY_ProgressRange;
			float3 _RotateX_ProgressRange;
			float _ParticleScale;
			float _CollectionStagger;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _MoveToFusionAxisEarly;
			float _OffsetFusionAxisVariance;
			float _OffsetFusionAxisStrength;
			float _MoveToFusionPoint_Variance;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Progress;
			float _RotateX_Variance;
			float _RotateX_Speed;
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

				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult668 = (float3(1.0 , 0.0 , 0.0));
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g118 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g118 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g118 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g118;
				float temp_output_675_0 = ( _RotateX_Speed * ( 1.0 + ( ( RandomViaX566 + RandomViaY641 ) * _RotateX_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Progress * saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float2 temp_cast_4 = (transform639.z).xx;
				float dotResult4_g119 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g119 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g119 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g119;
				float4 lerpResult631 = lerp( transform215 , float4( _FusionPoint , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 + ( _MoveToFusionPoint_Variance * RandomViaZ656 ) ) ) ));
				float4 MoveToFusionPoinOffset264 = lerpResult631;
				float dotResult604 = dot( ( MoveToFusionPoinOffset264 - float4( FusionPoint255 , 0.0 ) ) , float4( _FusionDirection , 0.0 ) );
				float3 ClosestPointOnFusionAxis615 = ( FusionPoint255 + ( dotResult604 * _FusionDirection ) );
				float2 appendResult638 = (float2(( ( RandomViaZ656 - 0.5 ) * 2.0 ) , ( ( RandomViaY641 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 lerpResult633 = lerp( MoveToFusionPoinOffset264 , float4( ( ClosestPointOnFusionAxis615 + float3( ( normalizeResult644 * _OffsetFusionAxisStrength * ( 1.0 + ( RandomViaZ656 * _OffsetFusionAxisVariance ) ) ) ,  0.0 ) ) , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 / ( 1.0 - _MoveToFusionAxisEarly ) ) ) ));
				float4 MovedToFusionAxisWithOffsetg646 = lerpResult633;
				float3 rotatedValue669 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult668 ), radians( ( temp_output_675_0 + ( temp_output_675_0 * _TimeParameters.x ) ) ) );
				float4 temp_output_687_0 = ( float4( rotatedValue669 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 RotatedXAroundClosesAxisPointOffset665 = temp_output_687_0;
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 rotatedValue785 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedALLAroundClosesAxisPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 temp_output_647_0 = ( ( float4( 0,0,0,0 ) + ( RotatedXAroundClosesAxisPointOffset665 * ( Progress_Delayed619 <= _RotateX_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateX_ProgressRange.x) * (1.0 - 0.0) / (_RotateX_ProgressRange.y - _RotateX_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateX_ProgressRange.y) * (0.0 - 1.0) / (_RotateX_ProgressRange.z - _RotateX_ProgressRange.y)) ) ) ) + ( RotatedYAroundClosesAxisPointOffset790 * ( Progress_Delayed619 <= _RotateY_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateY_ProgressRange.x) * (1.0 - 0.0) / (_RotateY_ProgressRange.y - _RotateY_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateY_ProgressRange.y) * (0.0 - 1.0) / (_RotateY_ProgressRange.z - _RotateY_ProgressRange.y)) ) ) ) + ( RotatedALLAroundClosesAxisPointOffset818 * ( Progress_Delayed619 <= _RotateZ_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.x) * (1.0 - 0.0) / (_RotateZ_ProgressRange.y - _RotateZ_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.y) * (0.0 - 1.0) / (_RotateZ_ProgressRange.z - _RotateZ_ProgressRange.y)) ) ) ) ) + MovedToFusionAxisWithOffsetg646 );
				float temp_output_25_0_g120 = saturate( _CollectionStagger );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_16 = (transform267.x).xx;
				float dotResult4_g121 = dot( temp_cast_16 , float2( 12.9898,78.233 ) );
				float lerpResult10_g121 = lerp( 0.0 , temp_output_25_0_g120 , frac( ( sin( dotResult4_g121 ) * 43758.55 ) ));
				float temp_output_4_0_g120 = lerpResult10_g121;
				float CollectionAdjusted219 = saturate( (0.0 + (0.0 - temp_output_4_0_g120) * (1.0 - 0.0) / (( temp_output_4_0_g120 + ( 1.0 - temp_output_25_0_g120 ) ) - temp_output_4_0_g120)) );
				float4 lerpResult863 = lerp( temp_output_647_0 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( float4( objToWorld373 , 0.0 ) - transform562 ) + lerpResult863 ).xyz, 1 ) ).xyz;
				

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
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Progress * saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_2 = (transform639.z).xx;
				float dotResult4_g119 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g119 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g119 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g119;
				float4 lerpResult631 = lerp( transform215 , float4( _FusionPoint , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 + ( _MoveToFusionPoint_Variance * RandomViaZ656 ) ) ) ));
				float4 MoveToFusionPoinOffset264 = lerpResult631;
				float dotResult604 = dot( ( MoveToFusionPoinOffset264 - float4( FusionPoint255 , 0.0 ) ) , float4( _FusionDirection , 0.0 ) );
				float3 ClosestPointOnFusionAxis615 = ( FusionPoint255 + ( dotResult604 * _FusionDirection ) );
				float3 appendResult668 = (float3(1.0 , 0.0 , 0.0));
				float2 temp_cast_6 = (transform639.y).xx;
				float dotResult4_g118 = dot( temp_cast_6 , float2( 12.9898,78.233 ) );
				float lerpResult10_g118 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g118 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g118;
				float temp_output_675_0 = ( _RotateX_Speed * ( 1.0 + ( ( RandomViaX566 + RandomViaY641 ) * _RotateX_Variance ) ) );
				float2 appendResult638 = (float2(( ( RandomViaZ656 - 0.5 ) * 2.0 ) , ( ( RandomViaY641 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 lerpResult633 = lerp( MoveToFusionPoinOffset264 , float4( ( ClosestPointOnFusionAxis615 + float3( ( normalizeResult644 * _OffsetFusionAxisStrength * ( 1.0 + ( RandomViaZ656 * _OffsetFusionAxisVariance ) ) ) ,  0.0 ) ) , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 / ( 1.0 - _MoveToFusionAxisEarly ) ) ) ));
				float4 MovedToFusionAxisWithOffsetg646 = lerpResult633;
				float3 rotatedValue669 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult668 ), radians( ( temp_output_675_0 + ( temp_output_675_0 * _TimeParameters.x ) ) ) );
				float4 temp_output_687_0 = ( float4( rotatedValue669 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 RotatedXAroundClosesAxisPointOffset665 = temp_output_687_0;
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 rotatedValue785 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedALLAroundClosesAxisPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 temp_output_647_0 = ( ( float4( 0,0,0,0 ) + ( RotatedXAroundClosesAxisPointOffset665 * ( Progress_Delayed619 <= _RotateX_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateX_ProgressRange.x) * (1.0 - 0.0) / (_RotateX_ProgressRange.y - _RotateX_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateX_ProgressRange.y) * (0.0 - 1.0) / (_RotateX_ProgressRange.z - _RotateX_ProgressRange.y)) ) ) ) + ( RotatedYAroundClosesAxisPointOffset790 * ( Progress_Delayed619 <= _RotateY_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateY_ProgressRange.x) * (1.0 - 0.0) / (_RotateY_ProgressRange.y - _RotateY_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateY_ProgressRange.y) * (0.0 - 1.0) / (_RotateY_ProgressRange.z - _RotateY_ProgressRange.y)) ) ) ) + ( RotatedALLAroundClosesAxisPointOffset818 * ( Progress_Delayed619 <= _RotateZ_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.x) * (1.0 - 0.0) / (_RotateZ_ProgressRange.y - _RotateZ_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.y) * (0.0 - 1.0) / (_RotateZ_ProgressRange.z - _RotateZ_ProgressRange.y)) ) ) ) ) + MovedToFusionAxisWithOffsetg646 );
				float4 FinalPosition851 = temp_output_647_0;
				float temp_output_861_0 = ( distance( float4( ClosestPointOnFusionAxis615 , 0.0 ) , FinalPosition851 ) + distance( float4( FusionPoint255 , 0.0 ) , FinalPosition851 ) );
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
			float3 _RotateZ_ProgressRange;
			float3 _FusionDirection;
			float3 _RotateY_ProgressRange;
			float3 _RotateX_ProgressRange;
			float _ParticleScale;
			float _CollectionStagger;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _MoveToFusionAxisEarly;
			float _OffsetFusionAxisVariance;
			float _OffsetFusionAxisStrength;
			float _MoveToFusionPoint_Variance;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Progress;
			float _RotateX_Variance;
			float _RotateX_Speed;
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

				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult668 = (float3(1.0 , 0.0 , 0.0));
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g118 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g118 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g118 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g118;
				float temp_output_675_0 = ( _RotateX_Speed * ( 1.0 + ( ( RandomViaX566 + RandomViaY641 ) * _RotateX_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Progress * saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float2 temp_cast_4 = (transform639.z).xx;
				float dotResult4_g119 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g119 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g119 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g119;
				float4 lerpResult631 = lerp( transform215 , float4( _FusionPoint , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 + ( _MoveToFusionPoint_Variance * RandomViaZ656 ) ) ) ));
				float4 MoveToFusionPoinOffset264 = lerpResult631;
				float dotResult604 = dot( ( MoveToFusionPoinOffset264 - float4( FusionPoint255 , 0.0 ) ) , float4( _FusionDirection , 0.0 ) );
				float3 ClosestPointOnFusionAxis615 = ( FusionPoint255 + ( dotResult604 * _FusionDirection ) );
				float2 appendResult638 = (float2(( ( RandomViaZ656 - 0.5 ) * 2.0 ) , ( ( RandomViaY641 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 lerpResult633 = lerp( MoveToFusionPoinOffset264 , float4( ( ClosestPointOnFusionAxis615 + float3( ( normalizeResult644 * _OffsetFusionAxisStrength * ( 1.0 + ( RandomViaZ656 * _OffsetFusionAxisVariance ) ) ) ,  0.0 ) ) , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 / ( 1.0 - _MoveToFusionAxisEarly ) ) ) ));
				float4 MovedToFusionAxisWithOffsetg646 = lerpResult633;
				float3 rotatedValue669 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult668 ), radians( ( temp_output_675_0 + ( temp_output_675_0 * _TimeParameters.x ) ) ) );
				float4 temp_output_687_0 = ( float4( rotatedValue669 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 RotatedXAroundClosesAxisPointOffset665 = temp_output_687_0;
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 rotatedValue785 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedALLAroundClosesAxisPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 temp_output_647_0 = ( ( float4( 0,0,0,0 ) + ( RotatedXAroundClosesAxisPointOffset665 * ( Progress_Delayed619 <= _RotateX_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateX_ProgressRange.x) * (1.0 - 0.0) / (_RotateX_ProgressRange.y - _RotateX_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateX_ProgressRange.y) * (0.0 - 1.0) / (_RotateX_ProgressRange.z - _RotateX_ProgressRange.y)) ) ) ) + ( RotatedYAroundClosesAxisPointOffset790 * ( Progress_Delayed619 <= _RotateY_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateY_ProgressRange.x) * (1.0 - 0.0) / (_RotateY_ProgressRange.y - _RotateY_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateY_ProgressRange.y) * (0.0 - 1.0) / (_RotateY_ProgressRange.z - _RotateY_ProgressRange.y)) ) ) ) + ( RotatedALLAroundClosesAxisPointOffset818 * ( Progress_Delayed619 <= _RotateZ_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.x) * (1.0 - 0.0) / (_RotateZ_ProgressRange.y - _RotateZ_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.y) * (0.0 - 1.0) / (_RotateZ_ProgressRange.z - _RotateZ_ProgressRange.y)) ) ) ) ) + MovedToFusionAxisWithOffsetg646 );
				float temp_output_25_0_g120 = saturate( _CollectionStagger );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_16 = (transform267.x).xx;
				float dotResult4_g121 = dot( temp_cast_16 , float2( 12.9898,78.233 ) );
				float lerpResult10_g121 = lerp( 0.0 , temp_output_25_0_g120 , frac( ( sin( dotResult4_g121 ) * 43758.55 ) ));
				float temp_output_4_0_g120 = lerpResult10_g121;
				float CollectionAdjusted219 = saturate( (0.0 + (0.0 - temp_output_4_0_g120) * (1.0 - 0.0) / (( temp_output_4_0_g120 + ( 1.0 - temp_output_25_0_g120 ) ) - temp_output_4_0_g120)) );
				float4 lerpResult863 = lerp( temp_output_647_0 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( float4( objToWorld373 , 0.0 ) - transform562 ) + lerpResult863 ).xyz, 1 ) ).xyz;
				

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
			float3 _RotateZ_ProgressRange;
			float3 _FusionDirection;
			float3 _RotateY_ProgressRange;
			float3 _RotateX_ProgressRange;
			float _ParticleScale;
			float _CollectionStagger;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _MoveToFusionAxisEarly;
			float _OffsetFusionAxisVariance;
			float _OffsetFusionAxisStrength;
			float _MoveToFusionPoint_Variance;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Progress;
			float _RotateX_Variance;
			float _RotateX_Speed;
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

				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult668 = (float3(1.0 , 0.0 , 0.0));
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g118 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g118 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g118 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g118;
				float temp_output_675_0 = ( _RotateX_Speed * ( 1.0 + ( ( RandomViaX566 + RandomViaY641 ) * _RotateX_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Progress * saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float2 temp_cast_4 = (transform639.z).xx;
				float dotResult4_g119 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g119 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g119 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g119;
				float4 lerpResult631 = lerp( transform215 , float4( _FusionPoint , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 + ( _MoveToFusionPoint_Variance * RandomViaZ656 ) ) ) ));
				float4 MoveToFusionPoinOffset264 = lerpResult631;
				float dotResult604 = dot( ( MoveToFusionPoinOffset264 - float4( FusionPoint255 , 0.0 ) ) , float4( _FusionDirection , 0.0 ) );
				float3 ClosestPointOnFusionAxis615 = ( FusionPoint255 + ( dotResult604 * _FusionDirection ) );
				float2 appendResult638 = (float2(( ( RandomViaZ656 - 0.5 ) * 2.0 ) , ( ( RandomViaY641 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 lerpResult633 = lerp( MoveToFusionPoinOffset264 , float4( ( ClosestPointOnFusionAxis615 + float3( ( normalizeResult644 * _OffsetFusionAxisStrength * ( 1.0 + ( RandomViaZ656 * _OffsetFusionAxisVariance ) ) ) ,  0.0 ) ) , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 / ( 1.0 - _MoveToFusionAxisEarly ) ) ) ));
				float4 MovedToFusionAxisWithOffsetg646 = lerpResult633;
				float3 rotatedValue669 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult668 ), radians( ( temp_output_675_0 + ( temp_output_675_0 * _TimeParameters.x ) ) ) );
				float4 temp_output_687_0 = ( float4( rotatedValue669 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 RotatedXAroundClosesAxisPointOffset665 = temp_output_687_0;
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 rotatedValue785 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedALLAroundClosesAxisPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 temp_output_647_0 = ( ( float4( 0,0,0,0 ) + ( RotatedXAroundClosesAxisPointOffset665 * ( Progress_Delayed619 <= _RotateX_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateX_ProgressRange.x) * (1.0 - 0.0) / (_RotateX_ProgressRange.y - _RotateX_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateX_ProgressRange.y) * (0.0 - 1.0) / (_RotateX_ProgressRange.z - _RotateX_ProgressRange.y)) ) ) ) + ( RotatedYAroundClosesAxisPointOffset790 * ( Progress_Delayed619 <= _RotateY_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateY_ProgressRange.x) * (1.0 - 0.0) / (_RotateY_ProgressRange.y - _RotateY_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateY_ProgressRange.y) * (0.0 - 1.0) / (_RotateY_ProgressRange.z - _RotateY_ProgressRange.y)) ) ) ) + ( RotatedALLAroundClosesAxisPointOffset818 * ( Progress_Delayed619 <= _RotateZ_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.x) * (1.0 - 0.0) / (_RotateZ_ProgressRange.y - _RotateZ_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.y) * (0.0 - 1.0) / (_RotateZ_ProgressRange.z - _RotateZ_ProgressRange.y)) ) ) ) ) + MovedToFusionAxisWithOffsetg646 );
				float temp_output_25_0_g120 = saturate( _CollectionStagger );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_16 = (transform267.x).xx;
				float dotResult4_g121 = dot( temp_cast_16 , float2( 12.9898,78.233 ) );
				float lerpResult10_g121 = lerp( 0.0 , temp_output_25_0_g120 , frac( ( sin( dotResult4_g121 ) * 43758.55 ) ));
				float temp_output_4_0_g120 = lerpResult10_g121;
				float CollectionAdjusted219 = saturate( (0.0 + (0.0 - temp_output_4_0_g120) * (1.0 - 0.0) / (( temp_output_4_0_g120 + ( 1.0 - temp_output_25_0_g120 ) ) - temp_output_4_0_g120)) );
				float4 lerpResult863 = lerp( temp_output_647_0 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( float4( objToWorld373 , 0.0 ) - transform562 ) + lerpResult863 ).xyz, 1 ) ).xyz;
				

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
			float3 _RotateZ_ProgressRange;
			float3 _FusionDirection;
			float3 _RotateY_ProgressRange;
			float3 _RotateX_ProgressRange;
			float _ParticleScale;
			float _CollectionStagger;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _MoveToFusionAxisEarly;
			float _OffsetFusionAxisVariance;
			float _OffsetFusionAxisStrength;
			float _MoveToFusionPoint_Variance;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Progress;
			float _RotateX_Variance;
			float _RotateX_Speed;
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

				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult668 = (float3(1.0 , 0.0 , 0.0));
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g118 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g118 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g118 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g118;
				float temp_output_675_0 = ( _RotateX_Speed * ( 1.0 + ( ( RandomViaX566 + RandomViaY641 ) * _RotateX_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Progress * saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float2 temp_cast_4 = (transform639.z).xx;
				float dotResult4_g119 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g119 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g119 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g119;
				float4 lerpResult631 = lerp( transform215 , float4( _FusionPoint , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 + ( _MoveToFusionPoint_Variance * RandomViaZ656 ) ) ) ));
				float4 MoveToFusionPoinOffset264 = lerpResult631;
				float dotResult604 = dot( ( MoveToFusionPoinOffset264 - float4( FusionPoint255 , 0.0 ) ) , float4( _FusionDirection , 0.0 ) );
				float3 ClosestPointOnFusionAxis615 = ( FusionPoint255 + ( dotResult604 * _FusionDirection ) );
				float2 appendResult638 = (float2(( ( RandomViaZ656 - 0.5 ) * 2.0 ) , ( ( RandomViaY641 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 lerpResult633 = lerp( MoveToFusionPoinOffset264 , float4( ( ClosestPointOnFusionAxis615 + float3( ( normalizeResult644 * _OffsetFusionAxisStrength * ( 1.0 + ( RandomViaZ656 * _OffsetFusionAxisVariance ) ) ) ,  0.0 ) ) , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 / ( 1.0 - _MoveToFusionAxisEarly ) ) ) ));
				float4 MovedToFusionAxisWithOffsetg646 = lerpResult633;
				float3 rotatedValue669 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult668 ), radians( ( temp_output_675_0 + ( temp_output_675_0 * _TimeParameters.x ) ) ) );
				float4 temp_output_687_0 = ( float4( rotatedValue669 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 RotatedXAroundClosesAxisPointOffset665 = temp_output_687_0;
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 rotatedValue785 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedALLAroundClosesAxisPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 temp_output_647_0 = ( ( float4( 0,0,0,0 ) + ( RotatedXAroundClosesAxisPointOffset665 * ( Progress_Delayed619 <= _RotateX_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateX_ProgressRange.x) * (1.0 - 0.0) / (_RotateX_ProgressRange.y - _RotateX_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateX_ProgressRange.y) * (0.0 - 1.0) / (_RotateX_ProgressRange.z - _RotateX_ProgressRange.y)) ) ) ) + ( RotatedYAroundClosesAxisPointOffset790 * ( Progress_Delayed619 <= _RotateY_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateY_ProgressRange.x) * (1.0 - 0.0) / (_RotateY_ProgressRange.y - _RotateY_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateY_ProgressRange.y) * (0.0 - 1.0) / (_RotateY_ProgressRange.z - _RotateY_ProgressRange.y)) ) ) ) + ( RotatedALLAroundClosesAxisPointOffset818 * ( Progress_Delayed619 <= _RotateZ_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.x) * (1.0 - 0.0) / (_RotateZ_ProgressRange.y - _RotateZ_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.y) * (0.0 - 1.0) / (_RotateZ_ProgressRange.z - _RotateZ_ProgressRange.y)) ) ) ) ) + MovedToFusionAxisWithOffsetg646 );
				float temp_output_25_0_g120 = saturate( _CollectionStagger );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_16 = (transform267.x).xx;
				float dotResult4_g121 = dot( temp_cast_16 , float2( 12.9898,78.233 ) );
				float lerpResult10_g121 = lerp( 0.0 , temp_output_25_0_g120 , frac( ( sin( dotResult4_g121 ) * 43758.55 ) ));
				float temp_output_4_0_g120 = lerpResult10_g121;
				float CollectionAdjusted219 = saturate( (0.0 + (0.0 - temp_output_4_0_g120) * (1.0 - 0.0) / (( temp_output_4_0_g120 + ( 1.0 - temp_output_25_0_g120 ) ) - temp_output_4_0_g120)) );
				float4 lerpResult863 = lerp( temp_output_647_0 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( float4( objToWorld373 , 0.0 ) - transform562 ) + lerpResult863 ).xyz, 1 ) ).xyz;
				

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
			float3 _RotateZ_ProgressRange;
			float3 _FusionDirection;
			float3 _RotateY_ProgressRange;
			float3 _RotateX_ProgressRange;
			float _ParticleScale;
			float _CollectionStagger;
			float _RotateZ_Variance;
			float _RotateZ_Speed;
			float _RotateY_Variance;
			float _RotateY_Speed;
			float _MoveToFusionAxisEarly;
			float _OffsetFusionAxisVariance;
			float _OffsetFusionAxisStrength;
			float _MoveToFusionPoint_Variance;
			float _ProgressDelay_Variance;
			float _DelayCatchUpTime;
			float _ProgressDelay_Min;
			float _Progress;
			float _RotateX_Variance;
			float _RotateX_Speed;
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

				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 appendResult668 = (float3(1.0 , 0.0 , 0.0));
				float4 transform333 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_1 = (transform333.x).xx;
				float dotResult4_g117 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g117 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g117 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g117;
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_2 = (transform639.y).xx;
				float dotResult4_g118 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g118 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g118 ) * 43758.55 ) ));
				float RandomViaY641 = lerpResult10_g118;
				float temp_output_675_0 = ( _RotateX_Speed * ( 1.0 + ( ( RandomViaX566 + RandomViaY641 ) * _RotateX_Variance ) ) );
				float3 FusionPoint255 = _FusionPoint;
				float4 transform215 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float temp_output_436_0 = ( _DelayCatchUpTime + _ProgressDelay_Variance );
				float DelayCapped438 = ( temp_output_436_0 > 1.0 ? ( _ProgressDelay_Variance - ( temp_output_436_0 - 1.0 ) ) : _ProgressDelay_Variance );
				float temp_output_567_0 = ( saturate( (_ProgressDelay_Min + (RandomViaX566 - 0.0) * (1.0 - _ProgressDelay_Min) / (1.0 - 0.0)) ) * DelayCapped438 );
				float DelayCatchUp449 = _DelayCatchUpTime;
				float Progress_Delayed619 = ( _Progress * saturate( ( _Progress >= temp_output_567_0 ? (0.0 + (( _Progress - temp_output_567_0 ) - 0.0) * (1.0 - 0.0) / (DelayCatchUp449 - 0.0)) : 0.0 ) ) );
				float2 temp_cast_4 = (transform639.z).xx;
				float dotResult4_g119 = dot( temp_cast_4 , float2( 12.9898,78.233 ) );
				float lerpResult10_g119 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g119 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g119;
				float4 lerpResult631 = lerp( transform215 , float4( _FusionPoint , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 + ( _MoveToFusionPoint_Variance * RandomViaZ656 ) ) ) ));
				float4 MoveToFusionPoinOffset264 = lerpResult631;
				float dotResult604 = dot( ( MoveToFusionPoinOffset264 - float4( FusionPoint255 , 0.0 ) ) , float4( _FusionDirection , 0.0 ) );
				float3 ClosestPointOnFusionAxis615 = ( FusionPoint255 + ( dotResult604 * _FusionDirection ) );
				float2 appendResult638 = (float2(( ( RandomViaZ656 - 0.5 ) * 2.0 ) , ( ( RandomViaY641 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float4 lerpResult633 = lerp( MoveToFusionPoinOffset264 , float4( ( ClosestPointOnFusionAxis615 + float3( ( normalizeResult644 * _OffsetFusionAxisStrength * ( 1.0 + ( RandomViaZ656 * _OffsetFusionAxisVariance ) ) ) ,  0.0 ) ) , 0.0 ) , saturate( ( Progress_Delayed619 * ( 1.0 / ( 1.0 - _MoveToFusionAxisEarly ) ) ) ));
				float4 MovedToFusionAxisWithOffsetg646 = lerpResult633;
				float3 rotatedValue669 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult668 ), radians( ( temp_output_675_0 + ( temp_output_675_0 * _TimeParameters.x ) ) ) );
				float4 temp_output_687_0 = ( float4( rotatedValue669 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 RotatedXAroundClosesAxisPointOffset665 = temp_output_687_0;
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateY_Variance ) ) );
				float3 rotatedValue785 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float4 RotatedYAroundClosesAxisPointOffset790 = ( float4( rotatedValue785 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaY641 ) * _RotateZ_Variance ) ) );
				float3 rotatedValue806 = RotateAroundAxis( ClosestPointOnFusionAxis615, MovedToFusionAxisWithOffsetg646.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float4 RotatedALLAroundClosesAxisPointOffset818 = ( float4( rotatedValue806 , 0.0 ) - MovedToFusionAxisWithOffsetg646 );
				float4 temp_output_647_0 = ( ( float4( 0,0,0,0 ) + ( RotatedXAroundClosesAxisPointOffset665 * ( Progress_Delayed619 <= _RotateX_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateX_ProgressRange.x) * (1.0 - 0.0) / (_RotateX_ProgressRange.y - _RotateX_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateX_ProgressRange.y) * (0.0 - 1.0) / (_RotateX_ProgressRange.z - _RotateX_ProgressRange.y)) ) ) ) + ( RotatedYAroundClosesAxisPointOffset790 * ( Progress_Delayed619 <= _RotateY_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateY_ProgressRange.x) * (1.0 - 0.0) / (_RotateY_ProgressRange.y - _RotateY_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateY_ProgressRange.y) * (0.0 - 1.0) / (_RotateY_ProgressRange.z - _RotateY_ProgressRange.y)) ) ) ) + ( RotatedALLAroundClosesAxisPointOffset818 * ( Progress_Delayed619 <= _RotateZ_ProgressRange.y ? saturate( (0.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.x) * (1.0 - 0.0) / (_RotateZ_ProgressRange.y - _RotateZ_ProgressRange.x)) ) : saturate( (1.0 + (Progress_Delayed619 - _RotateZ_ProgressRange.y) * (0.0 - 1.0) / (_RotateZ_ProgressRange.z - _RotateZ_ProgressRange.y)) ) ) ) ) + MovedToFusionAxisWithOffsetg646 );
				float temp_output_25_0_g120 = saturate( _CollectionStagger );
				float4 transform267 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_16 = (transform267.x).xx;
				float dotResult4_g121 = dot( temp_cast_16 , float2( 12.9898,78.233 ) );
				float lerpResult10_g121 = lerp( 0.0 , temp_output_25_0_g120 , frac( ( sin( dotResult4_g121 ) * 43758.55 ) ));
				float temp_output_4_0_g120 = lerpResult10_g121;
				float CollectionAdjusted219 = saturate( (0.0 + (0.0 - temp_output_4_0_g120) * (1.0 - 0.0) / (( temp_output_4_0_g120 + ( 1.0 - temp_output_25_0_g120 ) ) - temp_output_4_0_g120)) );
				float4 lerpResult863 = lerp( temp_output_647_0 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( ( float4( objToWorld373 , 0.0 ) - transform562 ) + lerpResult863 ).xyz, 1 ) ).xyz;
				

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
Node;AmplifyShaderEditor.CommentaryNode;661;3258.221,-4387.824;Inherit;False;1515.317;661.3047;Comment;15;644;652;650;651;638;636;653;643;645;635;659;660;658;657;689;OffsetFromFusionAxis;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;616;3199.48,-3537.206;Inherit;False;2195.834;649.1808;Comment;17;615;614;597;601;605;606;599;604;617;623;625;626;627;629;632;633;690;MoveToFusionAxis;0.5263038,0.5465524,0.9528302,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;590;7184.54,-541.3126;Inherit;False;742.2939;484.9873;vertices;6;373;371;372;374;562;563;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;565;4171.773,-359.4157;Inherit;False;1983.349;986.3488;Comment;21;856;857;852;855;854;526;515;531;525;533;529;538;540;539;519;537;532;516;858;859;861;CenterColor;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;459;1110.046,-2039.087;Inherit;False;2744.508;923.9855;Comment;32;2;3;4;5;6;7;8;9;440;434;333;449;330;436;439;435;458;452;129;447;455;441;438;426;450;566;567;619;622;847;848;849;Delay;0.6597723,0.2776922,0.8773585,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;265;1990.444,-3554.395;Inherit;False;1103.841;648.4066;;9;194;255;215;264;631;620;840;841;844;Move To Fusion Point;0.6653691,0.8490566,0.6127626,1;0;0
Node;AmplifyShaderEditor.StickyNoteNode;440;1907.9,-1989.087;Inherit;False;228.8146;130.6501;;;1,1,1,1;Make sure that delay + catchup is within one, otherwise some particles might not catch up before the end.$;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;436;1652.258,-1868.701;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DistanceOpNode;516;4468.436,-179.3884;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;532;4531.541,163.2056;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;537;4264.074,-83.41417;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;519;4221.773,64.34956;Inherit;False;Property;_FusionColorRange;FusionColorRange;21;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;539;4808.566,-89.84628;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;540;5118.568,65.15376;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;538;4988.566,-156.8464;Inherit;False;2;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;533;4932.485,67.09077;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;525;4343.246,290.7799;Inherit;False;Property;_FusionColorRangeFallOff;FusionColorRangeFallOff;22;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;531;4722.776,287.1691;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;597;3455.683,-3072.225;Inherit;False;Property;_FusionDirection;FusionDirection;31;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.DotProductOpNode;604;3721.505,-3261.403;Inherit;False;2;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;599;3515.207,-3227.349;Inherit;False;2;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;215;1988.355,-3510.395;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector3Node;194;1998.014,-3286.743;Inherit;False;Property;_FusionPoint;FusionPoint;30;0;Create;False;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.LerpOp;631;2499.04,-3412.823;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LerpOp;633;4992.971,-3433.331;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;614;3845.15,-3358.996;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;606;3851.868,-3199.955;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;605;4069.167,-3296.565;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.NormalizeNode;644;4073.225,-4153.917;Inherit;False;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;652;3567.593,-4281.306;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;650;3759.595,-4306.305;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;651;3712.594,-4149.307;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;638;3915.298,-4198.826;Inherit;False;FLOAT2;4;0;FLOAT;0.6;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;653;3523.592,-4123.307;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;643;3308.221,-4232.917;Inherit;False;641;RandomViaY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;659;3957.533,-3896.392;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;658;3706.15,-3939.136;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;663;6002.688,-5538.258;Inherit;False;1864.76;1237.109;Rotation;20;665;692;668;670;688;669;687;666;675;673;683;671;676;674;672;772;773;774;775;680;Rotate Around Fusion Axis Point X;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;660;4156.532,-3880.392;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;645;4436.28,-4068.815;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;689;4374.895,-3846.959;Inherit;False;619;Progress Delayed;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;636;3362.293,-4337.824;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;601;3273.48,-3336.718;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;632;3287.565,-3461.739;Inherit;False;264;MoveToFusionPoinOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;617;4156.457,-3176.887;Inherit;False;Property;_MoveToFusionAxisEarly;MoveToFusionAxisEarly;25;0;Create;True;0;0;0;False;0;False;0;0;0;0.99;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;690;4600.582,-3364.925;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT2;0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;626;4662.728,-3161.726;Inherit;False;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;625;4452.666,-3087.765;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;627;4717.739,-3279.484;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;629;4876.429,-3186.929;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;623;4440.285,-3219.22;Inherit;False;619;Progress Delayed;1;0;OBJECT;;False;1;FLOAT;0
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
Node;AmplifyShaderEditor.RangedFloatNode;490;-321.5751,-3080.933;Inherit;False;Property;_OffsetFromFusionPointVariation;OffsetFromFusionPointVariation;24;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;489;-66.42014,-3612.85;Inherit;False;Property;_OffsetFromFusionPointStrength;OffsetFromFusionPointStrength;23;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;672;6439.436,-4964.431;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;674;6999.703,-5381.247;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;683;6445.647,-5482.331;Inherit;False;Property;_RotateX_Speed;RotateX_Speed;13;0;Create;True;0;0;0;False;0;False;0;0;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;673;6798.793,-4998.371;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;675;6724.083,-5343.249;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;687;7114.067,-4562.461;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;669;6721.034,-4747.988;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;688;6260.4,-4508.648;Inherit;False;646;MovedToFusionAxisWithOffsetg;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;670;6279.941,-4662.53;Inherit;False;615;ClosestPointOnFusionAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;668;6507.005,-4811.433;Inherit;False;FLOAT3;4;0;FLOAT;1;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;692;7526.704,-4533.166;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RadiansOpNode;666;7217.676,-5254.156;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;775;6434.588,-5184.961;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;671;6580.149,-5318.95;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;772;6002.596,-5271.092;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;774;6224.645,-5246.871;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;773;6012.017,-5159.395;Inherit;False;641;RandomViaY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;680;6024.914,-5075.97;Inherit;False;Property;_RotateX_Variance;RotateX_Variance;14;0;Create;True;0;0;0;False;0;False;10;1.2;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;777;6001.128,-4202.071;Inherit;False;1864.76;1237.109;Rotation;19;797;796;795;793;792;791;790;788;787;786;785;784;783;782;781;780;779;778;794;Rotate Around Fusion Axis Point X;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;665;7341.915,-4829.665;Inherit;False;RotatedXAroundClosesAxisPointOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
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
Node;AmplifyShaderEditor.RangedFloatNode;781;6444.087,-4146.144;Inherit;False;Property;_RotateY_Speed;RotateY_Speed;16;0;Create;True;0;0;0;False;0;False;0;0;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;787;6278.381,-3326.343;Inherit;False;615;ClosestPointOnFusionAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;788;6505.445,-3475.246;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;797;6023.354,-3741.79;Inherit;False;Property;_RotateY_Variance;RotateY_Variance;17;0;Create;True;0;0;0;False;0;False;10;1.2;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;790;7300.373,-3468.726;Inherit;False;RotatedYAroundClosesAxisPointOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CommentaryNode;799;7881.625,-2858.828;Inherit;False;1864.76;1237.109;Rotation;19;818;817;816;815;813;812;811;810;809;808;807;806;805;804;803;802;801;800;839;Rotate Around Fusion Axis Point Z;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.SimpleTimeNode;800;8318.375,-2285.001;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;801;8878.643,-2701.817;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;802;8104.52,-2283.101;Inherit;False;545;UseTime;1;0;OBJECT;;False;1;INT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;803;8677.732,-2318.941;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;804;8603.021,-2663.819;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;805;8993.006,-1883.03;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RadiansOpNode;807;9096.615,-2574.726;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;808;8313.527,-2505.531;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;809;8459.088,-2639.52;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;810;8103.583,-2567.441;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;811;7890.956,-2479.965;Inherit;False;641;RandomViaY;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;371;7452.546,-361.2617;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;563;7794.919,-257.7143;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.PosVertexDataNode;372;7206.829,-488.6636;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TransformPositionNode;373;7602.583,-505.8896;Inherit;True;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;562;7554.548,-219.1773;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;589;7989.412,-911.0822;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;374;7208.806,-231.4285;Inherit;False;Property;_ParticleScale;ParticleScale;5;0;Create;True;0;0;0;False;0;False;0.3;0.2;0;0.3;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;646;5156.89,-3424.083;Inherit;False;MovedToFusionAxisWithOffsetg;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SaturateNode;766;6053.993,-2460.95;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;769;6052.493,-2285.95;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;763;5528.497,-2448.295;Inherit;False;Property;_RotateX_ProgressRange;RotateX_ProgressRange;12;0;Create;True;0;0;0;False;0;False;0.2,0.3,1;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.TFHCRemapNode;765;5865.644,-2450.952;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;768;5868.299,-2275.95;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;767;6233.993,-2593.95;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;699;5607.75,-2585.613;Inherit;False;619;Progress Delayed;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;771;6629.454,-2563.376;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;527;6656.028,-513.6217;Inherit;False;526;FusionColorAmount;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;523;6749.612,-887.7932;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;834;6627.953,-2302.049;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SaturateNode;820;5936.037,-1989.546;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;821;5934.537,-1814.546;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;823;5747.688,-1979.548;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;824;5750.343,-1804.546;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;825;6116.037,-2122.546;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;826;5489.793,-2114.209;Inherit;False;619;Progress Delayed;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;822;5410.541,-1975.227;Inherit;False;Property;_RotateY_ProgressRange;RotateY_ProgressRange;15;0;Create;True;0;0;0;False;0;False;0.2,0.3,1;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SaturateNode;827;6113.483,-1447.521;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;828;6111.983,-1272.521;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;830;5925.134,-1437.523;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;831;5927.79,-1262.521;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;832;6293.483,-1580.521;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;833;5667.24,-1572.184;Inherit;False;619;Progress Delayed;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;647;7770.169,-1283.424;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;648;6217.613,-2685.783;Inherit;False;665;RotatedXAroundClosesAxisPointOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;835;6248.142,-2299.579;Inherit;False;790;RotatedYAroundClosesAxisPointOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;806;8741.036,-2111.354;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;812;7897.837,-2703.874;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;813;8324.586,-2802.901;Inherit;False;Property;_RotateZ_Speed;RotateZ_Speed;19;0;Create;True;0;0;0;False;0;False;0;0;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;817;7903.852,-2398.547;Inherit;False;Property;_RotateZ_Variance;RotateZ_Variance;20;0;Create;True;0;0;0;False;0;False;10;1.2;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;816;8512.547,-2176.927;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;818;9180.871,-2125.483;Inherit;False;RotatedALLAroundClosesAxisPointOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector3Node;829;5589.993,-1434.866;Inherit;False;Property;_RotateZ_ProgressRange;RotateZ_ProgressRange;18;0;Create;True;0;0;0;False;0;False;0.2,0.3,1;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;815;8084.193,-1926.784;Inherit;False;615;ClosestPointOnFusionAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;786;6258.84,-3172.461;Inherit;False;646;MovedToFusionAxisWithOffsetg;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;838;7012.709,-2539.375;Inherit;False;4;4;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT4;0,0,0,0;False;3;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;819;6584.024,-2768.446;Inherit;False;646;MovedToFusionAxisWithOffsetg;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;839;8187.876,-1807.977;Inherit;False;646;MovedToFusionAxisWithOffsetg;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;837;6311.478,-1789.449;Inherit;False;818;RotatedALLAroundClosesAxisPointOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;836;6828.536,-1733.894;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;264;2763.369,-3264.517;Inherit;False;MoveToFusionPoinOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;255;2208.916,-3199.915;Inherit;False;FusionPoint;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;842;2630.591,-3162.341;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;844;2491.162,-3250.711;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;841;2553.334,-3062.659;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;840;2170.992,-2954.657;Inherit;False;Property;_MoveToFusionPoint_Variance;MoveToFusionPoint_Variance;28;0;Create;True;0;0;0;False;0;False;0;0;0;3;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;845;2191.866,-2868.021;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;846;2440.37,-2947.957;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;620;2124.35,-3104.865;Inherit;False;619;Progress Delayed;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;8;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;9;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;619;3658.772,-1885.414;Inherit;False;Progress Delayed;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;129;2247.838,-1980.186;Inherit;False;Property;_Progress;Progress;1;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;434;1160.88,-1926.267;Inherit;False;Property;_DelayCatchUpTime;DelayCatchUpTime;11;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;449;1529.725,-1971.735;Inherit;False;DelayCatchUp;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;439;1711.248,-1577.098;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;458;1498.921,-1492.87;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;435;1872.905,-1746.011;Inherit;False;2;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;333;2060.835,-1771.136;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;426;2245.976,-1731.339;Inherit;False;Random Range;-1;;117;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;847;2037.215,-1232.458;Inherit;False;Property;_ProgressDelay_Min;ProgressDelay_Min;2;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;622;3509.688,-1874.409;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;452;3374.843,-1740.409;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;441;3196.109,-1764.014;Inherit;False;3;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;447;3068.555,-1564.195;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;455;2875.164,-1711.886;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;450;2863.167,-1370.804;Inherit;False;449;DelayCatchUp;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;567;2752.004,-1502.432;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;849;2625.571,-1356.349;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;848;2397.317,-1301.306;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;330;1176.429,-1764.053;Inherit;False;Property;_ProgressDelay_Variance;ProgressDelay_Variance;10;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;438;1952.007,-1404.678;Inherit;False;DelayCapped;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;566;2420.233,-1710.243;Inherit;False;RandomViaX;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;615;4242.575,-3346.793;Inherit;False;ClosestPointOnFusionAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TransformPositionNode;26;8319.985,-913.1738;Inherit;False;World;Object;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;8649.215,-1011.256;Float;False;True;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;Potato 3 Axis Rotation;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;1;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;;0;0;Standard;23;Surface;0;0;  Blend;0;0;Two Sided;1;0;Forward Only;0;0;Cast Shadows;0;638332276799351495;  Use Shadow Threshold;0;0;Receive Shadows;0;638332276827847575;GPU Instancing;1;638332271287541535;LOD CrossFade;0;0;Built-in Fog;0;0;DOTS Instancing;0;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Vertex Position,InvertActionOnDeselection;0;638274534278721169;0;10;False;True;False;True;False;False;True;True;True;False;False;;False;0
Node;AmplifyShaderEditor.SaturateNode;529;5289.352,136.1183;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;852;5299.484,-78.13897;Inherit;False;Property;_FusionAxisColor_MinDistance;FusionAxisColor_MinDistance;8;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;854;5296.541,-0.2754244;Inherit;False;Property;_FusionAxisColor_MaxDistance;FusionAxisColor_MaxDistance;9;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;859;5872.437,-93.28534;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;857;5846.007,-311.7293;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;10;6405.323,-1065.245;Inherit;False;Property;_MainColor;MainColor;6;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,1.017304,5.934532,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;526;5829.425,157.0957;Inherit;False;FusionColorAmount;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;856;5127.069,-207.4219;Inherit;False;851;FinalPosition;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ColorNode;518;6405.967,-800.4439;Inherit;False;Property;_FusionAxisColor;FusionAxisColor;7;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,1.017304,5.934532,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TFHCRemapNode;858;5672.638,-78.12498;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;850;5120.002,-456.617;Inherit;False;615;ClosestPointOnFusionAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DistanceOpNode;855;5443.452,-273.5324;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DistanceOpNode;860;5451.848,-429.3688;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;861;5650.313,-341.7578;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;515;5097.616,-343.7786;Inherit;False;255;FusionPoint;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;851;8035.731,-1240.855;Inherit;False;FinalPosition;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;862;7557.26,-994.1721;Inherit;False;219;CollectionAdjusted;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;863;7885.49,-1077.491;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;266;7345.276,-1249.522;Inherit;False;646;MovedToFusionAxisWithOffsetg;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector3Node;225;7549.958,-794.5245;Inherit;False;Property;_CollectionPoint;CollectionPoint;29;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;498;618.8747,-3334.052;Inherit;False;OffsetAroundFusionPoint;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;635;3906.255,-4009.437;Inherit;False;Property;_OffsetFusionAxisStrength;OffsetFusionAxisStrength;26;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;657;3652.451,-3839.322;Inherit;False;Property;_OffsetFusionAxisVariance;OffsetFusionAxisVariance;27;0;Create;True;0;0;0;False;0;False;0;0;0;2.17;0;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;267;-556.2869,-1830.473;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.IntNode;541;-336.4528,-2318.454;Inherit;False;Property;_UseTime;UseTime;0;0;Create;True;0;0;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.RangedFloatNode;230;-605.7039,-2017.574;Inherit;False;Property;_CollectionStagger;CollectionStagger;4;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;218;-662.8572,-1933.795;Inherit;False;Property;_Collection;Collection;3;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;641;-94.48891,-1587.806;Inherit;False;RandomViaY;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;649;-327.8249,-1610.539;Inherit;False;Random Range;-1;;118;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;639;-582.4109,-1634.699;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;654;-375.8849,-1405.039;Inherit;False;Random Range;-1;;119;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;656;-98.39711,-1438.398;Inherit;False;RandomViaZ;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;390;-227.6579,-1991.188;Inherit;False;Stagger;-1;;120;93a439cb4f13e644e8dcf460c2df1f83;0;3;28;FLOAT;0;False;9;FLOAT;0;False;11;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;219;51.1129,-1961.651;Inherit;False;CollectionAdjusted;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;545;-42.61488,-2281.764;Inherit;False;UseTime;-1;True;1;0;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.GetLocalVarNode;676;6146.582,-4931.531;Inherit;False;545;UseTime;1;0;OBJECT;;False;1;INT;0
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
WireConnection;604;0;599;0
WireConnection;604;1;597;0
WireConnection;599;0;632;0
WireConnection;599;1;601;0
WireConnection;631;0;215;0
WireConnection;631;1;194;0
WireConnection;631;2;844;0
WireConnection;633;0;632;0
WireConnection;633;1;690;0
WireConnection;633;2;629;0
WireConnection;606;0;604;0
WireConnection;606;1;597;0
WireConnection;605;0;614;0
WireConnection;605;1;606;0
WireConnection;644;0;638;0
WireConnection;652;0;636;0
WireConnection;650;0;652;0
WireConnection;651;0;653;0
WireConnection;638;0;650;0
WireConnection;638;1;651;0
WireConnection;653;0;643;0
WireConnection;659;0;658;0
WireConnection;659;1;657;0
WireConnection;660;1;659;0
WireConnection;645;0;644;0
WireConnection;645;1;635;0
WireConnection;645;2;660;0
WireConnection;690;0;615;0
WireConnection;690;1;645;0
WireConnection;626;1;625;0
WireConnection;625;0;617;0
WireConnection;627;0;623;0
WireConnection;627;1;626;0
WireConnection;629;0;627;0
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
WireConnection;692;1;687;0
WireConnection;666;0;674;0
WireConnection;775;0;774;0
WireConnection;775;1;680;0
WireConnection;671;1;775;0
WireConnection;774;0;772;0
WireConnection;774;1;773;0
WireConnection;665;0;687;0
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
WireConnection;790;0;784;0
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
WireConnection;589;0;563;0
WireConnection;589;1;863;0
WireConnection;646;0;633;0
WireConnection;766;0;765;0
WireConnection;769;0;768;0
WireConnection;765;0;699;0
WireConnection;765;1;763;1
WireConnection;765;2;763;2
WireConnection;768;0;699;0
WireConnection;768;1;763;2
WireConnection;768;2;763;3
WireConnection;767;0;699;0
WireConnection;767;1;763;2
WireConnection;767;2;766;0
WireConnection;767;3;769;0
WireConnection;771;0;648;0
WireConnection;771;1;767;0
WireConnection;523;0;10;0
WireConnection;523;1;518;0
WireConnection;523;2;527;0
WireConnection;834;0;835;0
WireConnection;834;1;825;0
WireConnection;820;0;823;0
WireConnection;821;0;824;0
WireConnection;823;0;826;0
WireConnection;823;1;822;1
WireConnection;823;2;822;2
WireConnection;824;0;826;0
WireConnection;824;1;822;2
WireConnection;824;2;822;3
WireConnection;825;0;826;0
WireConnection;825;1;822;2
WireConnection;825;2;820;0
WireConnection;825;3;821;0
WireConnection;827;0;830;0
WireConnection;828;0;831;0
WireConnection;830;0;833;0
WireConnection;830;1;829;1
WireConnection;830;2;829;2
WireConnection;831;0;833;0
WireConnection;831;1;829;2
WireConnection;831;2;829;3
WireConnection;832;0;833;0
WireConnection;832;1;829;2
WireConnection;832;2;827;0
WireConnection;832;3;828;0
WireConnection;647;0;838;0
WireConnection;647;1;266;0
WireConnection;806;0;816;0
WireConnection;806;1;807;0
WireConnection;806;2;815;0
WireConnection;806;3;839;0
WireConnection;818;0;805;0
WireConnection;838;1;771;0
WireConnection;838;2;834;0
WireConnection;838;3;836;0
WireConnection;836;0;837;0
WireConnection;836;1;832;0
WireConnection;264;0;631;0
WireConnection;255;0;194;0
WireConnection;842;0;620;0
WireConnection;842;1;841;0
WireConnection;844;0;842;0
WireConnection;841;1;846;0
WireConnection;846;0;840;0
WireConnection;846;1;845;0
WireConnection;619;0;622;0
WireConnection;449;0;434;0
WireConnection;439;0;330;0
WireConnection;439;1;458;0
WireConnection;458;0;436;0
WireConnection;435;0;436;0
WireConnection;435;2;439;0
WireConnection;435;3;330;0
WireConnection;426;1;333;1
WireConnection;622;0;129;0
WireConnection;622;1;452;0
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
WireConnection;566;0;426;0
WireConnection;615;0;605;0
WireConnection;26;0;589;0
WireConnection;1;2;523;0
WireConnection;1;5;26;0
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
WireConnection;851;0;647;0
WireConnection;863;0;647;0
WireConnection;863;1;225;0
WireConnection;863;2;862;0
WireConnection;498;0;499;0
WireConnection;641;0;649;0
WireConnection;649;1;639;2
WireConnection;654;1;639;3
WireConnection;656;0;654;0
WireConnection;390;9;230;0
WireConnection;390;11;267;1
WireConnection;219;0;390;0
WireConnection;545;0;541;0
ASEEND*/
//CHKSM=FD6D161B91157272EC0E179D9AAF761C32A9FA5A