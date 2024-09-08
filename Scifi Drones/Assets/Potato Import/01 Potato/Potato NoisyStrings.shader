// Made with Amplify Shader Editor v1.9.2.1
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Backup Potato Noisy Strings"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_Transformation("Transformation", Range( 0 , 1)) = 0
		_ParticleScale("ParticleScale", Range( 0 , 0.3)) = 0.3
		[HDR]_SmackColor("SmackColor", Color) = (0.2042542,0.5284038,0.9622642,0)
		[HDR]_MainColor("MainColor", Color) = (0.2042542,0.5284038,0.9622642,0)
		_TransformationPosition("TransformationPosition", Vector) = (0,0,0,0)
		_Collection("Collection", Range( 0 , 1)) = 0
		_CollectionStagger("CollectionStagger", Range( 0 , 1)) = 0
		_CollectionPoint("CollectionPoint", Vector) = (0,0,0,0)
		_SmackCount("SmackCount", Range( 0 , 40)) = 8
		_SmackRadius("SmackRadius", Range( 0 , 2)) = 0.49
		_Radius("Radius", Range( 0 , 2)) = 0.49
		_RotateX("RotateX", Float) = 0
		_RotateY("RotateY", Float) = 0
		_RotateZ("RotateZ", Float) = 0
		_RotateZPreEntry("RotateZPreEntry", Float) = 0
		_MoveToSphereTime("MoveToSphereTime", Range( 0 , 1)) = 0.22
		_PositionNoise("PositionNoise", Range( 0 , 0.1)) = 0
		_MoveToSphereNoise("MoveToSphereNoise", Vector) = (0,0,0,0)
		_MoveToSphereNoiseScale("MoveToSphereNoiseScale", Range( 0.1 , 20)) = 0.1
		_FinalRotationX("FinalRotationX", Float) = 0
		_FinalRotationZ("FinalRotationZ", Float) = 0
		_StaggerRange("StaggerRange", Vector) = (0.1,0.2,0,0)
		_Float1("Float 1", Range( 0 , 20)) = 0


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
			float4 _MainColor;
			float4 _SmackColor;
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float3 _MoveToSphereNoise;
			float2 _StaggerRange;
			float _RotateZ;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _Radius;
			float _FinalRotationX;
			float _FinalRotationZ;
			float _MoveToSphereNoiseScale;
			float _SmackRadius;
			float _PositionNoise;
			float _RotateZPreEntry;
			float _RotateX;
			float _RotateY;
			float _MoveToSphereTime;
			float _Transformation;
			float _SmackCount;
			float _Float1;
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
			
					float2 voronoihash1176( float2 p )
					{
						
						p = float2( dot( p, float2( 127.1, 311.7 ) ), dot( p, float2( 269.5, 183.3 ) ) );
						return frac( sin( p ) *43758.5453);
					}
			
					float voronoi1176( float2 v, float time, inout float2 id, inout float2 mr, float smoothness, inout float2 smoothId )
					{
						float2 n = floor( v );
						float2 f = frac( v );
						float F1 = 8.0;
						float F2 = 8.0; float2 mg = 0;
						for ( int j = -1; j <= 1; j++ )
						{
							for ( int i = -1; i <= 1; i++ )
						 	{
						 		float2 g = float2( i, j );
						 		float2 o = voronoihash1176( n + g );
								o = ( sin( time + o * 6.2831 ) * 0.5 + 0.5 ); float2 r = f - g - o;
								float d = 0.5 * dot( r, r );
						 		if( d<F1 ) {
						 			F2 = F1;
						 			F1 = d; mg = g; mr = r; id = o;
						 		} else if( d<F2 ) {
						 			F2 = d;
						
						 		}
						 	}
						}
						return F1;
					}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float lerpResult1204 = lerp( _StaggerRange.x , _StaggerRange.y , _Transformation);
				float temp_output_25_0_g141 = saturate( lerpResult1204 );
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.x).xx;
				float dotResult4_g145 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g145 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g145 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g145;
				float2 temp_cast_1 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float UnravelTimeRelative1120 = saturate( (0.0 + (TransformationStaggered1075 - _MoveToSphereTime) * (1.0 - 0.0) / (1.0 - _MoveToSphereTime)) );
				float3 TransformationPosition264 = _TransformationPosition;
				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MoveToSphereTimeRelative1119 = saturate( (0.0 + (TransformationStaggered1075 - 0.0) * (1.0 - 0.0) / (_MoveToSphereTime - 0.0)) );
				float2 temp_cast_2 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float4 appendResult1136 = (float4(( RandomViaX566 * _PositionNoise ) , 0.0 , ( RandomViaZ656 * _PositionNoise ) , 0.0));
				float3 break1100 = TransformationPosition264;
				float temp_output_1145_0 = ( RandomViaX566 >= 0.5 ? 1.0 : -1.0 );
				float temp_output_1207_0 = (0.0 + (sin( ( ( _SmackCount * ( 2.0 * PI ) * UnravelTimeRelative1120 ) + ( 0.5 * PI ) ) ) - -1.0) * (1.0 - 0.0) / (1.0 - -1.0));
				float4 appendResult1099 = (float4(break1100.x , ( break1100.y + ( _SmackRadius * temp_output_1145_0 * temp_output_1207_0 ) ) , break1100.z , 0.0));
				float3 rotatedValue1167 = RotateAroundAxis( TransformationPosition264, ( appendResult1136 + appendResult1099 ).xyz, normalize( float3( 0,0,1 ) ), ( MoveToSphereTimeRelative1119 * _RotateZPreEntry ) );
				float time1176 = 34.41;
				float2 voronoiSmoothId1176 = 0;
				float2 temp_cast_4 = (MoveToSphereTimeRelative1119).xx;
				float2 coords1176 = temp_cast_4 * _MoveToSphereNoiseScale;
				float2 id1176 = 0;
				float2 uv1176 = 0;
				float voroi1176 = voronoi1176( coords1176, time1176, id1176, uv1176, 0, voronoiSmoothId1176 );
				float4 lerpResult1158 = lerp( transform872 , float4( ( rotatedValue1167 + ( (-1.0 + (voroi1176 - 0.0) * (1.0 - -1.0) / (0.43 - 0.0)) * _MoveToSphereNoise * sin( ( MoveToSphereTimeRelative1119 * PI ) ) * temp_output_1145_0 ) ) , 0.0 ) , MoveToSphereTimeRelative1119);
				float4 LerpedToSphereEntry1162 = lerpResult1158;
				float3 rotatedValue1093 = RotateAroundAxis( TransformationPosition264, LerpedToSphereEntry1162.xyz, normalize( float3( 1,0,0 ) ), ( _RotateX * UnravelTimeRelative1120 ) );
				float3 rotatedValue1106 = RotateAroundAxis( TransformationPosition264, rotatedValue1093, float3( 0,1,0 ), ( _RotateY * UnravelTimeRelative1120 ) );
				float3 rotatedValue1110 = RotateAroundAxis( TransformationPosition264, rotatedValue1106, normalize( float3( 0,0,1 ) ), ( _RotateZ * UnravelTimeRelative1120 ) );
				float3 break1234 = TransformationPosition264;
				float3 appendResult1233 = (float3(break1234.x , ( break1234.y + ( _Radius * ( RandomViaX566 > 0.5 ? 1.0 : -1.0 ) * RandomViaZ656 ) ) , break1234.z));
				float3 rotatedValue1163 = RotateAroundAxis( TransformationPosition264, appendResult1233, normalize( float3( 1,0,0 ) ), ( _FinalRotationX * _TimeParameters.x * RandomViaZ656 ) );
				float3 rotatedValue1196 = RotateAroundAxis( TransformationPosition264, rotatedValue1163, normalize( float3( 0,0,1 ) ), ( _FinalRotationZ * _TimeParameters.x * RandomViaZ656 ) );
				float temp_output_1210_0 = ( _SmackCount * UnravelTimeRelative1120 );
				float temp_output_1222_0 = round( temp_output_1210_0 );
				float LerpToFinalRotation1225 = ( temp_output_1222_0 / _SmackCount );
				float temp_output_1213_0 = ( RandomViaX566 < LerpToFinalRotation1225 ? 1.0 : 0.0 );
				float3 lerpResult1192 = lerp( rotatedValue1110 , rotatedValue1196 , temp_output_1213_0);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_7 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_7 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float3 lerpResult1045 = lerp( lerpResult1192 , _CollectionPoint , CollectionAdjusted219);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( float4( lerpResult1045 , 0.0 ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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

				float lerpResult1204 = lerp( _StaggerRange.x , _StaggerRange.y , _Transformation);
				float temp_output_25_0_g141 = saturate( lerpResult1204 );
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.x).xx;
				float dotResult4_g145 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g145 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g145 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g145;
				float2 temp_cast_1 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float UnravelTimeRelative1120 = saturate( (0.0 + (TransformationStaggered1075 - _MoveToSphereTime) * (1.0 - 0.0) / (1.0 - _MoveToSphereTime)) );
				float temp_output_1207_0 = (0.0 + (sin( ( ( _SmackCount * ( 2.0 * PI ) * UnravelTimeRelative1120 ) + ( 0.5 * PI ) ) ) - -1.0) * (1.0 - 0.0) / (1.0 - -1.0));
				float SmackRelative1241 = temp_output_1207_0;
				float temp_output_1210_0 = ( _SmackCount * UnravelTimeRelative1120 );
				float temp_output_1222_0 = round( temp_output_1210_0 );
				float LerpToFinalRotation1225 = ( temp_output_1222_0 / _SmackCount );
				float4 lerpResult1249 = lerp( _MainColor , _SmackColor , ( pow( ( 1.0 - SmackRelative1241 ) , _Float1 ) * ( RandomViaX566 < LerpToFinalRotation1225 ? 0.0 : 1.0 ) ));
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = lerpResult1249.rgb;
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
			float4 _MainColor;
			float4 _SmackColor;
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float3 _MoveToSphereNoise;
			float2 _StaggerRange;
			float _RotateZ;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _Radius;
			float _FinalRotationX;
			float _FinalRotationZ;
			float _MoveToSphereNoiseScale;
			float _SmackRadius;
			float _PositionNoise;
			float _RotateZPreEntry;
			float _RotateX;
			float _RotateY;
			float _MoveToSphereTime;
			float _Transformation;
			float _SmackCount;
			float _Float1;
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
			
					float2 voronoihash1176( float2 p )
					{
						
						p = float2( dot( p, float2( 127.1, 311.7 ) ), dot( p, float2( 269.5, 183.3 ) ) );
						return frac( sin( p ) *43758.5453);
					}
			
					float voronoi1176( float2 v, float time, inout float2 id, inout float2 mr, float smoothness, inout float2 smoothId )
					{
						float2 n = floor( v );
						float2 f = frac( v );
						float F1 = 8.0;
						float F2 = 8.0; float2 mg = 0;
						for ( int j = -1; j <= 1; j++ )
						{
							for ( int i = -1; i <= 1; i++ )
						 	{
						 		float2 g = float2( i, j );
						 		float2 o = voronoihash1176( n + g );
								o = ( sin( time + o * 6.2831 ) * 0.5 + 0.5 ); float2 r = f - g - o;
								float d = 0.5 * dot( r, r );
						 		if( d<F1 ) {
						 			F2 = F1;
						 			F1 = d; mg = g; mr = r; id = o;
						 		} else if( d<F2 ) {
						 			F2 = d;
						
						 		}
						 	}
						}
						return F1;
					}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float lerpResult1204 = lerp( _StaggerRange.x , _StaggerRange.y , _Transformation);
				float temp_output_25_0_g141 = saturate( lerpResult1204 );
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.x).xx;
				float dotResult4_g145 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g145 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g145 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g145;
				float2 temp_cast_1 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float UnravelTimeRelative1120 = saturate( (0.0 + (TransformationStaggered1075 - _MoveToSphereTime) * (1.0 - 0.0) / (1.0 - _MoveToSphereTime)) );
				float3 TransformationPosition264 = _TransformationPosition;
				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MoveToSphereTimeRelative1119 = saturate( (0.0 + (TransformationStaggered1075 - 0.0) * (1.0 - 0.0) / (_MoveToSphereTime - 0.0)) );
				float2 temp_cast_2 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float4 appendResult1136 = (float4(( RandomViaX566 * _PositionNoise ) , 0.0 , ( RandomViaZ656 * _PositionNoise ) , 0.0));
				float3 break1100 = TransformationPosition264;
				float temp_output_1145_0 = ( RandomViaX566 >= 0.5 ? 1.0 : -1.0 );
				float temp_output_1207_0 = (0.0 + (sin( ( ( _SmackCount * ( 2.0 * PI ) * UnravelTimeRelative1120 ) + ( 0.5 * PI ) ) ) - -1.0) * (1.0 - 0.0) / (1.0 - -1.0));
				float4 appendResult1099 = (float4(break1100.x , ( break1100.y + ( _SmackRadius * temp_output_1145_0 * temp_output_1207_0 ) ) , break1100.z , 0.0));
				float3 rotatedValue1167 = RotateAroundAxis( TransformationPosition264, ( appendResult1136 + appendResult1099 ).xyz, normalize( float3( 0,0,1 ) ), ( MoveToSphereTimeRelative1119 * _RotateZPreEntry ) );
				float time1176 = 34.41;
				float2 voronoiSmoothId1176 = 0;
				float2 temp_cast_4 = (MoveToSphereTimeRelative1119).xx;
				float2 coords1176 = temp_cast_4 * _MoveToSphereNoiseScale;
				float2 id1176 = 0;
				float2 uv1176 = 0;
				float voroi1176 = voronoi1176( coords1176, time1176, id1176, uv1176, 0, voronoiSmoothId1176 );
				float4 lerpResult1158 = lerp( transform872 , float4( ( rotatedValue1167 + ( (-1.0 + (voroi1176 - 0.0) * (1.0 - -1.0) / (0.43 - 0.0)) * _MoveToSphereNoise * sin( ( MoveToSphereTimeRelative1119 * PI ) ) * temp_output_1145_0 ) ) , 0.0 ) , MoveToSphereTimeRelative1119);
				float4 LerpedToSphereEntry1162 = lerpResult1158;
				float3 rotatedValue1093 = RotateAroundAxis( TransformationPosition264, LerpedToSphereEntry1162.xyz, normalize( float3( 1,0,0 ) ), ( _RotateX * UnravelTimeRelative1120 ) );
				float3 rotatedValue1106 = RotateAroundAxis( TransformationPosition264, rotatedValue1093, float3( 0,1,0 ), ( _RotateY * UnravelTimeRelative1120 ) );
				float3 rotatedValue1110 = RotateAroundAxis( TransformationPosition264, rotatedValue1106, normalize( float3( 0,0,1 ) ), ( _RotateZ * UnravelTimeRelative1120 ) );
				float3 break1234 = TransformationPosition264;
				float3 appendResult1233 = (float3(break1234.x , ( break1234.y + ( _Radius * ( RandomViaX566 > 0.5 ? 1.0 : -1.0 ) * RandomViaZ656 ) ) , break1234.z));
				float3 rotatedValue1163 = RotateAroundAxis( TransformationPosition264, appendResult1233, normalize( float3( 1,0,0 ) ), ( _FinalRotationX * _TimeParameters.x * RandomViaZ656 ) );
				float3 rotatedValue1196 = RotateAroundAxis( TransformationPosition264, rotatedValue1163, normalize( float3( 0,0,1 ) ), ( _FinalRotationZ * _TimeParameters.x * RandomViaZ656 ) );
				float temp_output_1210_0 = ( _SmackCount * UnravelTimeRelative1120 );
				float temp_output_1222_0 = round( temp_output_1210_0 );
				float LerpToFinalRotation1225 = ( temp_output_1222_0 / _SmackCount );
				float temp_output_1213_0 = ( RandomViaX566 < LerpToFinalRotation1225 ? 1.0 : 0.0 );
				float3 lerpResult1192 = lerp( rotatedValue1110 , rotatedValue1196 , temp_output_1213_0);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_7 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_7 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float3 lerpResult1045 = lerp( lerpResult1192 , _CollectionPoint , CollectionAdjusted219);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( float4( lerpResult1045 , 0.0 ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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
			float4 _MainColor;
			float4 _SmackColor;
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float3 _MoveToSphereNoise;
			float2 _StaggerRange;
			float _RotateZ;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _Radius;
			float _FinalRotationX;
			float _FinalRotationZ;
			float _MoveToSphereNoiseScale;
			float _SmackRadius;
			float _PositionNoise;
			float _RotateZPreEntry;
			float _RotateX;
			float _RotateY;
			float _MoveToSphereTime;
			float _Transformation;
			float _SmackCount;
			float _Float1;
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
			
					float2 voronoihash1176( float2 p )
					{
						
						p = float2( dot( p, float2( 127.1, 311.7 ) ), dot( p, float2( 269.5, 183.3 ) ) );
						return frac( sin( p ) *43758.5453);
					}
			
					float voronoi1176( float2 v, float time, inout float2 id, inout float2 mr, float smoothness, inout float2 smoothId )
					{
						float2 n = floor( v );
						float2 f = frac( v );
						float F1 = 8.0;
						float F2 = 8.0; float2 mg = 0;
						for ( int j = -1; j <= 1; j++ )
						{
							for ( int i = -1; i <= 1; i++ )
						 	{
						 		float2 g = float2( i, j );
						 		float2 o = voronoihash1176( n + g );
								o = ( sin( time + o * 6.2831 ) * 0.5 + 0.5 ); float2 r = f - g - o;
								float d = 0.5 * dot( r, r );
						 		if( d<F1 ) {
						 			F2 = F1;
						 			F1 = d; mg = g; mr = r; id = o;
						 		} else if( d<F2 ) {
						 			F2 = d;
						
						 		}
						 	}
						}
						return F1;
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

				float lerpResult1204 = lerp( _StaggerRange.x , _StaggerRange.y , _Transformation);
				float temp_output_25_0_g141 = saturate( lerpResult1204 );
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.x).xx;
				float dotResult4_g145 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g145 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g145 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g145;
				float2 temp_cast_1 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float UnravelTimeRelative1120 = saturate( (0.0 + (TransformationStaggered1075 - _MoveToSphereTime) * (1.0 - 0.0) / (1.0 - _MoveToSphereTime)) );
				float3 TransformationPosition264 = _TransformationPosition;
				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MoveToSphereTimeRelative1119 = saturate( (0.0 + (TransformationStaggered1075 - 0.0) * (1.0 - 0.0) / (_MoveToSphereTime - 0.0)) );
				float2 temp_cast_2 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float4 appendResult1136 = (float4(( RandomViaX566 * _PositionNoise ) , 0.0 , ( RandomViaZ656 * _PositionNoise ) , 0.0));
				float3 break1100 = TransformationPosition264;
				float temp_output_1145_0 = ( RandomViaX566 >= 0.5 ? 1.0 : -1.0 );
				float temp_output_1207_0 = (0.0 + (sin( ( ( _SmackCount * ( 2.0 * PI ) * UnravelTimeRelative1120 ) + ( 0.5 * PI ) ) ) - -1.0) * (1.0 - 0.0) / (1.0 - -1.0));
				float4 appendResult1099 = (float4(break1100.x , ( break1100.y + ( _SmackRadius * temp_output_1145_0 * temp_output_1207_0 ) ) , break1100.z , 0.0));
				float3 rotatedValue1167 = RotateAroundAxis( TransformationPosition264, ( appendResult1136 + appendResult1099 ).xyz, normalize( float3( 0,0,1 ) ), ( MoveToSphereTimeRelative1119 * _RotateZPreEntry ) );
				float time1176 = 34.41;
				float2 voronoiSmoothId1176 = 0;
				float2 temp_cast_4 = (MoveToSphereTimeRelative1119).xx;
				float2 coords1176 = temp_cast_4 * _MoveToSphereNoiseScale;
				float2 id1176 = 0;
				float2 uv1176 = 0;
				float voroi1176 = voronoi1176( coords1176, time1176, id1176, uv1176, 0, voronoiSmoothId1176 );
				float4 lerpResult1158 = lerp( transform872 , float4( ( rotatedValue1167 + ( (-1.0 + (voroi1176 - 0.0) * (1.0 - -1.0) / (0.43 - 0.0)) * _MoveToSphereNoise * sin( ( MoveToSphereTimeRelative1119 * PI ) ) * temp_output_1145_0 ) ) , 0.0 ) , MoveToSphereTimeRelative1119);
				float4 LerpedToSphereEntry1162 = lerpResult1158;
				float3 rotatedValue1093 = RotateAroundAxis( TransformationPosition264, LerpedToSphereEntry1162.xyz, normalize( float3( 1,0,0 ) ), ( _RotateX * UnravelTimeRelative1120 ) );
				float3 rotatedValue1106 = RotateAroundAxis( TransformationPosition264, rotatedValue1093, float3( 0,1,0 ), ( _RotateY * UnravelTimeRelative1120 ) );
				float3 rotatedValue1110 = RotateAroundAxis( TransformationPosition264, rotatedValue1106, normalize( float3( 0,0,1 ) ), ( _RotateZ * UnravelTimeRelative1120 ) );
				float3 break1234 = TransformationPosition264;
				float3 appendResult1233 = (float3(break1234.x , ( break1234.y + ( _Radius * ( RandomViaX566 > 0.5 ? 1.0 : -1.0 ) * RandomViaZ656 ) ) , break1234.z));
				float3 rotatedValue1163 = RotateAroundAxis( TransformationPosition264, appendResult1233, normalize( float3( 1,0,0 ) ), ( _FinalRotationX * _TimeParameters.x * RandomViaZ656 ) );
				float3 rotatedValue1196 = RotateAroundAxis( TransformationPosition264, rotatedValue1163, normalize( float3( 0,0,1 ) ), ( _FinalRotationZ * _TimeParameters.x * RandomViaZ656 ) );
				float temp_output_1210_0 = ( _SmackCount * UnravelTimeRelative1120 );
				float temp_output_1222_0 = round( temp_output_1210_0 );
				float LerpToFinalRotation1225 = ( temp_output_1222_0 / _SmackCount );
				float temp_output_1213_0 = ( RandomViaX566 < LerpToFinalRotation1225 ? 1.0 : 0.0 );
				float3 lerpResult1192 = lerp( rotatedValue1110 , rotatedValue1196 , temp_output_1213_0);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_7 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_7 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float3 lerpResult1045 = lerp( lerpResult1192 , _CollectionPoint , CollectionAdjusted219);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( float4( lerpResult1045 , 0.0 ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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
			float4 _MainColor;
			float4 _SmackColor;
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float3 _MoveToSphereNoise;
			float2 _StaggerRange;
			float _RotateZ;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _Radius;
			float _FinalRotationX;
			float _FinalRotationZ;
			float _MoveToSphereNoiseScale;
			float _SmackRadius;
			float _PositionNoise;
			float _RotateZPreEntry;
			float _RotateX;
			float _RotateY;
			float _MoveToSphereTime;
			float _Transformation;
			float _SmackCount;
			float _Float1;
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
			
					float2 voronoihash1176( float2 p )
					{
						
						p = float2( dot( p, float2( 127.1, 311.7 ) ), dot( p, float2( 269.5, 183.3 ) ) );
						return frac( sin( p ) *43758.5453);
					}
			
					float voronoi1176( float2 v, float time, inout float2 id, inout float2 mr, float smoothness, inout float2 smoothId )
					{
						float2 n = floor( v );
						float2 f = frac( v );
						float F1 = 8.0;
						float F2 = 8.0; float2 mg = 0;
						for ( int j = -1; j <= 1; j++ )
						{
							for ( int i = -1; i <= 1; i++ )
						 	{
						 		float2 g = float2( i, j );
						 		float2 o = voronoihash1176( n + g );
								o = ( sin( time + o * 6.2831 ) * 0.5 + 0.5 ); float2 r = f - g - o;
								float d = 0.5 * dot( r, r );
						 		if( d<F1 ) {
						 			F2 = F1;
						 			F1 = d; mg = g; mr = r; id = o;
						 		} else if( d<F2 ) {
						 			F2 = d;
						
						 		}
						 	}
						}
						return F1;
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

				float lerpResult1204 = lerp( _StaggerRange.x , _StaggerRange.y , _Transformation);
				float temp_output_25_0_g141 = saturate( lerpResult1204 );
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.x).xx;
				float dotResult4_g145 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g145 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g145 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g145;
				float2 temp_cast_1 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float UnravelTimeRelative1120 = saturate( (0.0 + (TransformationStaggered1075 - _MoveToSphereTime) * (1.0 - 0.0) / (1.0 - _MoveToSphereTime)) );
				float3 TransformationPosition264 = _TransformationPosition;
				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MoveToSphereTimeRelative1119 = saturate( (0.0 + (TransformationStaggered1075 - 0.0) * (1.0 - 0.0) / (_MoveToSphereTime - 0.0)) );
				float2 temp_cast_2 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float4 appendResult1136 = (float4(( RandomViaX566 * _PositionNoise ) , 0.0 , ( RandomViaZ656 * _PositionNoise ) , 0.0));
				float3 break1100 = TransformationPosition264;
				float temp_output_1145_0 = ( RandomViaX566 >= 0.5 ? 1.0 : -1.0 );
				float temp_output_1207_0 = (0.0 + (sin( ( ( _SmackCount * ( 2.0 * PI ) * UnravelTimeRelative1120 ) + ( 0.5 * PI ) ) ) - -1.0) * (1.0 - 0.0) / (1.0 - -1.0));
				float4 appendResult1099 = (float4(break1100.x , ( break1100.y + ( _SmackRadius * temp_output_1145_0 * temp_output_1207_0 ) ) , break1100.z , 0.0));
				float3 rotatedValue1167 = RotateAroundAxis( TransformationPosition264, ( appendResult1136 + appendResult1099 ).xyz, normalize( float3( 0,0,1 ) ), ( MoveToSphereTimeRelative1119 * _RotateZPreEntry ) );
				float time1176 = 34.41;
				float2 voronoiSmoothId1176 = 0;
				float2 temp_cast_4 = (MoveToSphereTimeRelative1119).xx;
				float2 coords1176 = temp_cast_4 * _MoveToSphereNoiseScale;
				float2 id1176 = 0;
				float2 uv1176 = 0;
				float voroi1176 = voronoi1176( coords1176, time1176, id1176, uv1176, 0, voronoiSmoothId1176 );
				float4 lerpResult1158 = lerp( transform872 , float4( ( rotatedValue1167 + ( (-1.0 + (voroi1176 - 0.0) * (1.0 - -1.0) / (0.43 - 0.0)) * _MoveToSphereNoise * sin( ( MoveToSphereTimeRelative1119 * PI ) ) * temp_output_1145_0 ) ) , 0.0 ) , MoveToSphereTimeRelative1119);
				float4 LerpedToSphereEntry1162 = lerpResult1158;
				float3 rotatedValue1093 = RotateAroundAxis( TransformationPosition264, LerpedToSphereEntry1162.xyz, normalize( float3( 1,0,0 ) ), ( _RotateX * UnravelTimeRelative1120 ) );
				float3 rotatedValue1106 = RotateAroundAxis( TransformationPosition264, rotatedValue1093, float3( 0,1,0 ), ( _RotateY * UnravelTimeRelative1120 ) );
				float3 rotatedValue1110 = RotateAroundAxis( TransformationPosition264, rotatedValue1106, normalize( float3( 0,0,1 ) ), ( _RotateZ * UnravelTimeRelative1120 ) );
				float3 break1234 = TransformationPosition264;
				float3 appendResult1233 = (float3(break1234.x , ( break1234.y + ( _Radius * ( RandomViaX566 > 0.5 ? 1.0 : -1.0 ) * RandomViaZ656 ) ) , break1234.z));
				float3 rotatedValue1163 = RotateAroundAxis( TransformationPosition264, appendResult1233, normalize( float3( 1,0,0 ) ), ( _FinalRotationX * _TimeParameters.x * RandomViaZ656 ) );
				float3 rotatedValue1196 = RotateAroundAxis( TransformationPosition264, rotatedValue1163, normalize( float3( 0,0,1 ) ), ( _FinalRotationZ * _TimeParameters.x * RandomViaZ656 ) );
				float temp_output_1210_0 = ( _SmackCount * UnravelTimeRelative1120 );
				float temp_output_1222_0 = round( temp_output_1210_0 );
				float LerpToFinalRotation1225 = ( temp_output_1222_0 / _SmackCount );
				float temp_output_1213_0 = ( RandomViaX566 < LerpToFinalRotation1225 ? 1.0 : 0.0 );
				float3 lerpResult1192 = lerp( rotatedValue1110 , rotatedValue1196 , temp_output_1213_0);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_7 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_7 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float3 lerpResult1045 = lerp( lerpResult1192 , _CollectionPoint , CollectionAdjusted219);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( float4( lerpResult1045 , 0.0 ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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
			float4 _MainColor;
			float4 _SmackColor;
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float3 _MoveToSphereNoise;
			float2 _StaggerRange;
			float _RotateZ;
			float _ParticleScale;
			float _CollectionStagger;
			float _Collection;
			float _Radius;
			float _FinalRotationX;
			float _FinalRotationZ;
			float _MoveToSphereNoiseScale;
			float _SmackRadius;
			float _PositionNoise;
			float _RotateZPreEntry;
			float _RotateX;
			float _RotateY;
			float _MoveToSphereTime;
			float _Transformation;
			float _SmackCount;
			float _Float1;
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
			
					float2 voronoihash1176( float2 p )
					{
						
						p = float2( dot( p, float2( 127.1, 311.7 ) ), dot( p, float2( 269.5, 183.3 ) ) );
						return frac( sin( p ) *43758.5453);
					}
			
					float voronoi1176( float2 v, float time, inout float2 id, inout float2 mr, float smoothness, inout float2 smoothId )
					{
						float2 n = floor( v );
						float2 f = frac( v );
						float F1 = 8.0;
						float F2 = 8.0; float2 mg = 0;
						for ( int j = -1; j <= 1; j++ )
						{
							for ( int i = -1; i <= 1; i++ )
						 	{
						 		float2 g = float2( i, j );
						 		float2 o = voronoihash1176( n + g );
								o = ( sin( time + o * 6.2831 ) * 0.5 + 0.5 ); float2 r = f - g - o;
								float d = 0.5 * dot( r, r );
						 		if( d<F1 ) {
						 			F2 = F1;
						 			F1 = d; mg = g; mr = r; id = o;
						 		} else if( d<F2 ) {
						 			F2 = d;
						
						 		}
						 	}
						}
						return F1;
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

				float lerpResult1204 = lerp( _StaggerRange.x , _StaggerRange.y , _Transformation);
				float temp_output_25_0_g141 = saturate( lerpResult1204 );
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.x).xx;
				float dotResult4_g145 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g145 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g145 ) * 43758.55 ) ));
				float RandomViaX566 = lerpResult10_g145;
				float2 temp_cast_1 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float UnravelTimeRelative1120 = saturate( (0.0 + (TransformationStaggered1075 - _MoveToSphereTime) * (1.0 - 0.0) / (1.0 - _MoveToSphereTime)) );
				float3 TransformationPosition264 = _TransformationPosition;
				float4 transform872 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MoveToSphereTimeRelative1119 = saturate( (0.0 + (TransformationStaggered1075 - 0.0) * (1.0 - 0.0) / (_MoveToSphereTime - 0.0)) );
				float2 temp_cast_2 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float4 appendResult1136 = (float4(( RandomViaX566 * _PositionNoise ) , 0.0 , ( RandomViaZ656 * _PositionNoise ) , 0.0));
				float3 break1100 = TransformationPosition264;
				float temp_output_1145_0 = ( RandomViaX566 >= 0.5 ? 1.0 : -1.0 );
				float temp_output_1207_0 = (0.0 + (sin( ( ( _SmackCount * ( 2.0 * PI ) * UnravelTimeRelative1120 ) + ( 0.5 * PI ) ) ) - -1.0) * (1.0 - 0.0) / (1.0 - -1.0));
				float4 appendResult1099 = (float4(break1100.x , ( break1100.y + ( _SmackRadius * temp_output_1145_0 * temp_output_1207_0 ) ) , break1100.z , 0.0));
				float3 rotatedValue1167 = RotateAroundAxis( TransformationPosition264, ( appendResult1136 + appendResult1099 ).xyz, normalize( float3( 0,0,1 ) ), ( MoveToSphereTimeRelative1119 * _RotateZPreEntry ) );
				float time1176 = 34.41;
				float2 voronoiSmoothId1176 = 0;
				float2 temp_cast_4 = (MoveToSphereTimeRelative1119).xx;
				float2 coords1176 = temp_cast_4 * _MoveToSphereNoiseScale;
				float2 id1176 = 0;
				float2 uv1176 = 0;
				float voroi1176 = voronoi1176( coords1176, time1176, id1176, uv1176, 0, voronoiSmoothId1176 );
				float4 lerpResult1158 = lerp( transform872 , float4( ( rotatedValue1167 + ( (-1.0 + (voroi1176 - 0.0) * (1.0 - -1.0) / (0.43 - 0.0)) * _MoveToSphereNoise * sin( ( MoveToSphereTimeRelative1119 * PI ) ) * temp_output_1145_0 ) ) , 0.0 ) , MoveToSphereTimeRelative1119);
				float4 LerpedToSphereEntry1162 = lerpResult1158;
				float3 rotatedValue1093 = RotateAroundAxis( TransformationPosition264, LerpedToSphereEntry1162.xyz, normalize( float3( 1,0,0 ) ), ( _RotateX * UnravelTimeRelative1120 ) );
				float3 rotatedValue1106 = RotateAroundAxis( TransformationPosition264, rotatedValue1093, float3( 0,1,0 ), ( _RotateY * UnravelTimeRelative1120 ) );
				float3 rotatedValue1110 = RotateAroundAxis( TransformationPosition264, rotatedValue1106, normalize( float3( 0,0,1 ) ), ( _RotateZ * UnravelTimeRelative1120 ) );
				float3 break1234 = TransformationPosition264;
				float3 appendResult1233 = (float3(break1234.x , ( break1234.y + ( _Radius * ( RandomViaX566 > 0.5 ? 1.0 : -1.0 ) * RandomViaZ656 ) ) , break1234.z));
				float3 rotatedValue1163 = RotateAroundAxis( TransformationPosition264, appendResult1233, normalize( float3( 1,0,0 ) ), ( _FinalRotationX * _TimeParameters.x * RandomViaZ656 ) );
				float3 rotatedValue1196 = RotateAroundAxis( TransformationPosition264, rotatedValue1163, normalize( float3( 0,0,1 ) ), ( _FinalRotationZ * _TimeParameters.x * RandomViaZ656 ) );
				float temp_output_1210_0 = ( _SmackCount * UnravelTimeRelative1120 );
				float temp_output_1222_0 = round( temp_output_1210_0 );
				float LerpToFinalRotation1225 = ( temp_output_1222_0 / _SmackCount );
				float temp_output_1213_0 = ( RandomViaX566 < LerpToFinalRotation1225 ? 1.0 : 0.0 );
				float3 lerpResult1192 = lerp( rotatedValue1110 , rotatedValue1196 , temp_output_1213_0);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_7 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_7 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float3 lerpResult1045 = lerp( lerpResult1192 , _CollectionPoint , CollectionAdjusted219);
				float3 objToWorld373 = mul( GetObjectToWorldMatrix(), float4( ( v.vertex.xyz * _ParticleScale ), 1 ) ).xyz;
				float4 transform562 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float3 worldToObj26 = mul( GetWorldToObjectMatrix(), float4( ( float4( lerpResult1045 , 0.0 ) + ( float4( objToWorld373 , 0.0 ) - transform562 ) ).xyz, 1 ) ).xyz;
				

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
Node;AmplifyShaderEditor.CommentaryNode;1226;9704.854,-2316.152;Inherit;False;986.459;605.9408;;8;1196;1197;1190;1198;1194;1189;1163;1195;FinalPosition;0.1460492,0.3773585,0.1372374,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;1165;7849.459,-2278.458;Inherit;False;947.0654;655.1862;Sphere Entry Points;9;1099;1097;1145;1146;1100;1096;1141;1225;1095;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;1164;7995.732,-2695.102;Inherit;False;732.8574;376.7369;Position  Noise;6;1136;1135;1138;1133;1137;1132;Position Noise;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;661;3261.751,-4423.125;Inherit;False;1515.317;661.3047;Comment;20;644;652;650;651;638;636;653;643;645;635;659;660;657;632;884;888;889;890;893;658;OffsetFromFusionAxis;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;616;1170.652,-6725.051;Inherit;False;2195.834;649.1808;Comment;8;615;614;597;601;605;606;599;604;MoveToFusionAxis;0.5263038,0.5465524,0.9528302,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;590;11829.34,-1645.875;Inherit;False;742.2939;484.9873;vertices;6;373;371;372;374;562;563;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;565;4171.773,-359.4157;Inherit;False;1983.349;986.3488;Comment;21;856;857;852;855;854;526;515;531;525;533;529;538;540;539;519;537;532;516;858;859;861;CenterColor;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;265;5820.772,-1755.59;Inherit;False;1103.841;648.4066;;4;194;215;264;1083;Move To Fusion Point;0.6653691,0.8490566,0.6127626,1;0;0
Node;AmplifyShaderEditor.DistanceOpNode;516;4468.436,-179.3884;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;532;4531.541,163.2056;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;537;4264.074,-83.41417;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;519;4221.773,64.34956;Inherit;False;Property;_FusionColorRange;FusionColorRange;14;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;539;4808.566,-89.84628;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;540;5118.568,65.15376;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;538;4988.566,-156.8464;Inherit;False;2;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;533;4932.485,67.09077;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;525;4343.246,290.7799;Inherit;False;Property;_FusionColorRangeFallOff;FusionColorRangeFallOff;15;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;531;4722.776,287.1691;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;659;3961.063,-3931.692;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;663;3749.409,-7470.825;Inherit;False;1864.76;1237.109;Rotation;19;665;668;670;688;669;687;666;675;673;683;671;676;674;672;772;773;774;775;680;Rotate Around Fusion Axis Point X;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;777;5923.247,-4980.873;Inherit;False;1864.76;1237.109;Rotation;20;797;796;795;793;792;791;790;788;787;786;785;784;783;782;781;780;779;778;794;1086;Rotate Around Fusion Axis Point X;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;799;7859.362,-4968.857;Inherit;False;1864.76;1237.109;Rotation;21;818;817;816;815;813;812;811;810;809;808;807;806;805;804;803;801;839;878;877;894;800;Rotate Around Fusion Axis Point Z;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;803;8655.47,-4428.97;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;804;8580.759,-4773.848;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;805;8970.743,-3993.059;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;808;8291.265,-4615.56;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;810;8081.321,-4677.47;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
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
Node;AmplifyShaderEditor.GetLocalVarNode;515;5097.616,-343.7786;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;660;4202.961,-3901.392;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;883;4707.241,-4179.629;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;881;3787.144,-4590.234;Inherit;False;Property;_FusionPointOnAxisOffset;FusionPointOnAxisOffset;17;0;Create;True;0;0;0;False;0;False;0.0434969;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;889;3986.808,-4416.03;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;888;4353.458,-4353.105;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;890;4130.204,-4414.302;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;653;3535.76,-4165.518;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.StickyNoteNode;894;9164.535,-3902.143;Inherit;False;332.6025;127.3905;New Note;;1,1,1,1;The idea here is to only calculate offset, so it's much easier to add them together. If we take the final position and add various rotations, it will keep adding the offset from the spawnpoint$The alternative would be to rotate the entire position one after another, pluggin in the result of one rotation into the next one.;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;877;8102.014,-4081.661;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;878;8212.391,-4257.633;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DotProductOpNode;604;1692.677,-6449.249;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;605;2040.339,-6484.41;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;606;1859.702,-6334.335;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;601;1243.425,-6432.498;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleTimeNode;672;4186.157,-6896.999;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;674;4746.424,-7313.814;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;683;4192.368,-7414.898;Inherit;False;Property;_RotateX_Speed;RotateX_Speed;8;0;Create;True;0;0;0;False;0;False;0;0;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;673;4545.514,-6930.938;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;675;4470.804,-7275.816;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;687;4860.788,-6495.028;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;688;4007.121,-6441.215;Inherit;False;880;TransformationPositionWithRingOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;668;4253.726,-6744;Inherit;False;FLOAT3;4;0;FLOAT;1;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RadiansOpNode;666;4964.397,-7186.723;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;775;4181.309,-7117.528;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;671;4326.87,-7251.518;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;772;3749.317,-7203.659;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;774;3971.366,-7179.438;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;773;3758.738,-7091.962;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;680;3771.635,-7008.538;Inherit;False;Property;_RotateX_Variance;RotateX_Variance;9;0;Create;True;0;0;0;False;0;False;10;1.2;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;676;3893.303,-6864.098;Inherit;False;545;UseTime;1;0;OBJECT;;False;1;INT;0
Node;AmplifyShaderEditor.GetLocalVarNode;670;3821.301,-6672.097;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;665;5088.636,-6760.083;Inherit;False;RotatedXAroundClosesAxisPointOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;599;1533.501,-6536.839;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;597;1414.569,-6280.794;Inherit;False;Property;_FusionDirection;FusionDirection;20;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;614;1896.167,-6665.912;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;615;2317.098,-6632.957;Inherit;False;ClosestPointOnFusionAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;669;4498.267,-6604.27;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;801;8922.937,-4687.734;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;800;8346.432,-4425.702;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;817;7948.573,-4476.479;Inherit;False;Property;_RotateZ_Variance;RotateZ_Variance;13;0;Create;True;0;0;0;False;0;False;10;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;812;7875.574,-4813.903;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;813;8243.712,-4903.161;Inherit;False;Property;_RotateZ_Speed;RotateZ_Speed;12;0;Create;True;0;0;0;False;0;False;0;229;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;652;3531.116,-4309.168;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;650;3727.319,-4311.346;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;635;4005.986,-4066.838;Inherit;False;Property;_FusionPointRingOffsetStrength;FusionPointRingOffsetStrength;16;0;Create;True;0;0;0;False;0;False;0.0434969;0.405;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;657;3594.42,-3849.71;Inherit;False;Property;_FusionPointRingOffsetVariance;FusionPointRingOffsetVariance;18;0;Create;True;0;0;0;False;0;False;0;0;0;2.17;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;645;4348.176,-4043.881;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.BreakToComponentsNode;884;4519.02,-4148.945;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SimpleAddOpNode;879;4960.855,-4039.917;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;638;3988.828,-4216.127;Inherit;False;FLOAT2;4;0;FLOAT;0.6;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;651;3738.796,-4149.131;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalizeNode;644;4173.121,-4201.443;Inherit;False;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;658;3721.357,-3947.191;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;811;7843.693,-4641.994;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;215;5940.117,-1678.471;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;632;4437.426,-3864.773;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;880;5269.801,-4028.406;Inherit;False;TransformationPositionWithRingOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;8;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;9;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;545;4162.063,-2115.116;Inherit;False;UseTime;-1;True;1;0;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.FunctionNode;654;3888.296,-1290.296;Inherit;False;Random Range;-1;;134;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;656;4125.483,-1270.315;Inherit;False;RandomViaZ;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;541;3939.136,-2126.692;Inherit;False;Property;_UseTime;UseTime;0;0;Create;True;0;0;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.FunctionNode;1085;3888.474,-1430.683;Inherit;False;Random Range;-1;;139;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;893;3718.189,-4434.687;Inherit;False;Random Range;-1;;140;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;-1;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;636;3319.022,-4372.87;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;643;3283.913,-4156.916;Inherit;False;1082;randomViaYnew;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;839;8141.596,-3898.381;Inherit;False;880;TransformationPositionWithRingOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;815;7922.778,-4214.023;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;816;8528.284,-4340.956;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;809;8441.994,-4718.523;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;778;6359.995,-4407.046;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;779;6920.263,-4823.862;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;780;6146.141,-4405.146;Inherit;False;545;UseTime;1;0;OBJECT;;False;1;INT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;782;6719.352,-4440.986;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;783;6644.642,-4785.864;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;784;7034.625,-4005.078;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;785;6641.594,-4190.604;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RadiansOpNode;791;7138.235,-4696.771;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;792;6355.146,-4627.576;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;793;6500.708,-4761.565;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;795;6145.204,-4689.486;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;796;5932.577,-4602.01;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;794;6190.814,-4608.649;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;781;6366.206,-4924.946;Inherit;False;Property;_RotateY_Speed;RotateY_Speed;10;0;Create;True;0;0;0;False;0;False;0;249;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;797;5945.473,-4520.592;Inherit;False;Property;_RotateY_Variance;RotateY_Variance;11;0;Create;True;0;0;0;False;0;False;10;0.05;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;788;6279.563,-4270.048;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;818;9157.608,-4234.029;Inherit;False;RotatedZAroundFusionPointOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;786;6045.045,-3993.739;Inherit;False;880;TransformationPositionWithRingOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;787;6200.5,-4103.933;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;790;7222.492,-4247.528;Inherit;False;RotatedYAroundClosesAxisPointOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;1086;6082.091,-3865.821;Inherit;False;818;RotatedZAroundFusionPointOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;1070;4215.06,-998.3702;Inherit;False;Stagger;-1;;141;93a439cb4f13e644e8dcf460c2df1f83;0;3;28;FLOAT;0;False;9;FLOAT;0;False;11;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;1088;4455.288,-1805.048;Inherit;False;Stagger;-1;;143;93a439cb4f13e644e8dcf460c2df1f83;0;3;28;FLOAT;0;False;9;FLOAT;0;False;11;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;218;3925.724,-1876.804;Inherit;False;Property;_Collection;Collection;21;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1090;3896.708,-1787.538;Inherit;False;Property;_CollectionStagger;CollectionStagger;22;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;219;4752.999,-1792.74;Inherit;False;CollectionAdjusted;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;129;3757.641,-1153.977;Inherit;False;Property;_Transformation;Transformation;2;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;194;5941.247,-1449.82;Inherit;False;Property;_TransformationPosition;TransformationPosition;19;0;Create;False;0;0;0;False;0;False;0,0,0;0.007405923,1.85188,1.126751;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleSubtractOpNode;1083;6364.98,-1597.894;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;806;8718.773,-4221.383;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RadiansOpNode;807;9074.353,-4684.755;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1104;8558.088,-1003.784;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1107;8733.801,-1266.974;Inherit;False;Property;_RotateY;RotateY;28;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1109;8979.736,-1248.742;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1130;9185.822,-1315.193;Inherit;False;1120;UnravelTimeRelative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1126;8676.612,-1120.646;Inherit;False;1120;UnravelTimeRelative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;1118;7670.723,-1113.61;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1157;4197.708,-1136.147;Inherit;False;Transformation;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;264;6533.94,-1354.989;Inherit;False;TransformationPosition;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;1114;9218.937,-1166.626;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;1136;8550.593,-2598.336;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1135;8261.356,-2645.102;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1138;8328.499,-2454.965;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1133;8045.733,-2531.163;Inherit;False;Property;_PositionNoise;PositionNoise;32;0;Create;True;0;0;0;False;0;False;0;0;0;0.1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1137;8079.427,-2440.771;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1132;8057.367,-2630.539;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;1110;9664.552,-1193.093;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;1127;9264.145,-1423.133;Inherit;False;Property;_RotateZ;RotateZ;29;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1160;9160.403,-2392.381;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TFHCRemapNode;1117;7456.224,-1192.11;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1116;7019.661,-1055.087;Inherit;False;Property;_MoveToSphereTime;MoveToSphereTime;31;0;Create;True;0;0;0;False;0;False;0.22;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1094;8453.46,-820.7259;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleTimeNode;1183;8854.562,-2593.57;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1170;8998.026,-2969.166;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;1169;9130.215,-3155.741;Inherit;False;Property;_RotateZPreEntry;RotateZPreEntry;30;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1161;8800.55,-2815.49;Inherit;False;1119;MoveToSphereTimeRelative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;1123;7449.396,-966.0321;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;1185;7427.734,-743.8591;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;1124;7660.944,-965.7068;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;1186;7616.734,-734.8591;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1119;7925.472,-1169.059;Inherit;False;MoveToSphereTimeRelative;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1200;7933.05,-655.5086;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;1204;3951.649,-1024.302;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;1203;3658.363,-973.0402;Inherit;False;Property;_StaggerRange;StaggerRange;40;0;Create;True;0;0;0;False;0;False;0.1,0.2;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;1091;3570.596,-1064.729;Inherit;False;Property;_TransformationStagger;TransformationStagger;1;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1075;4568.327,-1090.92;Inherit;False;TransformationStaggered;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1199;7616.022,-599.9944;Inherit;False;Property;_MaxLerpToFinalRotation;MaxLerpToFinalRotation;36;0;Create;True;0;0;0;False;0;False;0.9492577;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1098;8345.873,-1098.389;Inherit;False;Property;_RotateX;RotateX;27;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1187;7935.69,-802.592;Inherit;False;LerpToFinalRotationRelative;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1184;7031.286,-624.5343;Inherit;False;Property;_StartLerpingToFinalRotation;StartLerpingToFinalRotation;35;0;Create;True;0;0;0;False;0;False;0.72;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1076;7023.141,-1269.525;Inherit;False;1075;TransformationStaggered;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1125;8269.564,-925.2287;Inherit;False;1120;UnravelTimeRelative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1115;8905.113,-971.9089;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;1093;8925.621,-800.3907;Inherit;False;True;4;0;FLOAT3;1,0,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;1106;9277.428,-1023.689;Inherit;False;False;4;0;FLOAT3;0,1,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1128;9504.994,-1346.153;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1074;3931.98,-885.5711;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1120;7944.443,-958.5683;Inherit;False;UnravelTimeRelative;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleRemainderNode;1220;8041.876,-2585.46;Inherit;False;2;0;INT;0;False;1;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;12673.89,-2156.453;Float;False;True;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;Backup Potato Noisy Strings;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;1;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;;0;0;Standard;23;Surface;0;0;  Blend;0;0;Two Sided;1;0;Forward Only;0;0;Cast Shadows;0;638332276799351495;  Use Shadow Threshold;0;0;Receive Shadows;0;638332276827847575;GPU Instancing;0;638445450479345352;LOD CrossFade;0;0;Built-in Fog;0;0;DOTS Instancing;0;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Vertex Position,InvertActionOnDeselection;0;638274534278721169;0;10;False;True;False;True;False;False;True;True;True;False;False;;False;0
Node;AmplifyShaderEditor.TransformPositionNode;26;12391.19,-2042.333;Inherit;False;World;Object;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;589;12261.9,-1879.923;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LerpOp;1045;11694.25,-1924.646;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;10536.74,-3038.017;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;1167;9780.746,-3377.586;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1177;10240.76,-2633.088;Inherit;False;4;4;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;872;9840.271,-3168.166;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.PiNode;1178;9689.969,-2816.453;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;1179;10087.07,-2742.157;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;1180;9634.478,-2623.578;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0.43;False;3;FLOAT;-1;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1172;10392.07,-3154.437;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;1173;9930.699,-2522.004;Inherit;False;Property;_MoveToSphereNoise;MoveToSphereNoise;33;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.LerpOp;1158;10229.82,-3021.908;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;1190;9901.499,-2267.045;Inherit;False;Property;_FinalRotationZ;FinalRotationZ;39;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1197;10096.44,-2260.255;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1194;9807.521,-2100.688;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;1191;9506.752,-2042.404;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1159;9504.287,-1841.965;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;1227;9560.125,-1759.39;Inherit;False;1162;LerpedToSphereEntry;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;1163;10025.06,-1951.401;Inherit;False;True;4;0;FLOAT3;1,0,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1162;10651.1,-2905.739;Inherit;False;LerpedToSphereEntry;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.VoronoiNode;1176;9191.912,-2156.854;Inherit;True;0;0;1;0;1;False;1;False;False;False;4;0;FLOAT2;0,0;False;1;FLOAT;34.41;False;2;FLOAT;4.4;False;3;FLOAT;0;False;3;FLOAT;0;FLOAT2;1;FLOAT2;2
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1168;9337.814,-3285.732;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1166;8433.994,-705.0024;Inherit;False;1162;LerpedToSphereEntry;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.PiNode;1217;6881.358,-2294.172;Inherit;False;1;0;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;1215;6912.109,-2463.004;Inherit;False;1;0;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1181;8966.967,-1881.569;Inherit;False;Property;_MoveToSphereNoiseScale;MoveToSphereNoiseScale;34;0;Create;True;0;0;0;False;0;False;0.1;0;0.1;20;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1146;8246.588,-2008.155;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;1145;8155.086,-1821.637;Inherit;False;3;4;0;FLOAT;0;False;1;FLOAT;0.5;False;2;FLOAT;1;False;3;FLOAT;-1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1141;7950.147,-1851.822;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;1099;8632.084,-2158.818;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.BreakToComponentsNode;1100;8279.508,-2217.712;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SimpleAddOpNode;1097;8481.267,-2226.121;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1230;10950.83,-2513.038;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;1196;10368.35,-2167.677;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;1198;9552.408,-2246.654;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;1233;11413.33,-2653.701;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;1236;10561.79,-2717.091;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1235;11173.08,-2558.324;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;1234;10891.6,-2692.442;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.Compare;1231;10787.57,-2367.449;Inherit;False;2;4;0;FLOAT;0;False;1;FLOAT;0.5;False;2;FLOAT;1;False;3;FLOAT;-1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1189;10024.93,-2111.921;Inherit;False;Property;_FinalRotationY;FinalRotationY;37;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1228;10577.4,-2594.039;Inherit;False;Property;_Radius;Radius;26;0;Create;True;0;0;0;False;0;False;0.49;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1195;9550.857,-1947.672;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1188;9514.176,-2145.747;Inherit;False;Property;_FinalRotationX;FinalRotationX;38;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1237;10609.92,-2497.626;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1232;10440.67,-2401.17;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;1013;11476.41,-1841.62;Inherit;False;Property;_CollectionPoint;CollectionPoint;23;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;1046;11603.72,-1662.825;Inherit;False;219;CollectionAdjusted;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1095;7921.516,-1959.311;Inherit;False;Property;_SmackRadius;SmackRadius;25;0;Create;True;0;0;0;False;0;False;0.49;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1096;7946.707,-2224.738;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1229;7382.244,-2505.63;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;1206;7496.914,-2494.154;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;1207;7620.713,-2351.644;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;-1;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1241;7804.845,-2411.114;Inherit;False;SmackRelative;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;10;11640.84,-2777.378;Inherit;False;Property;_MainColor;MainColor;5;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,0.9284637,6.115068,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;1248;11976.99,-2840.876;Inherit;False;Property;_SmackColor;SmackColor;4;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,0.9284637,6.115068,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;1249;12247.96,-2478.582;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;1245;11429.72,-2490.326;Inherit;False;1241;SmackRelative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;1250;11641.44,-2542.539;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1246;11478.72,-2390.326;Inherit;False;1241;SmackRelative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;1251;11693.44,-2435.539;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;1242;12066.36,-2202.711;Inherit;False;4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1247;12026.74,-2475.578;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1243;11658.08,-2163.808;Inherit;False;1225;LerpToFinalRotation;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1244;11771.49,-2211.625;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;1254;11867.44,-2374.539;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1255;11540.44,-2320.539;Inherit;False;Property;_Float1;Float 1;42;0;Create;True;0;0;0;False;0;False;0;0;0;20;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleRemainderNode;1256;7450.099,-1983.261;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;1223;7526.383,-2151.375;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1225;7709.459,-2074.252;Inherit;False;LerpToFinalRotation;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1216;7213.489,-2551.957;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1257;7639.918,-1878.391;Inherit;False;TimeAfterSmack;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RoundOpNode;1222;7289.1,-2136.715;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1205;6882.116,-2612.511;Inherit;False;Property;_SmackCount;SmackCount;24;0;Create;True;0;0;0;False;0;False;8;0;0;40;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1209;6794.944,-2180.543;Inherit;False;1120;UnravelTimeRelative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;1210;7121.948,-2240.479;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;1262;7248.648,-1950.459;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1263;7308.6,-2035.615;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;1240;11426.23,-1657.803;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Compare;1213;10805.23,-1611.943;Inherit;False;4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;1239;11254.33,-1612.888;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;1192;11176.82,-1844.637;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;1211;10367.37,-1659.656;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1193;10280.44,-1555.266;Inherit;False;1225;LerpToFinalRotation;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1238;10670.83,-903.5562;Inherit;False;Property;_ExplosionTimeOnSmack;ExplosionTimeOnSmack;41;0;Create;True;0;0;0;False;0;False;0.1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1258;10709.14,-1006.718;Inherit;False;1257;TimeAfterSmack;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;1264;11319.8,-1199.507;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TFHCRemapNode;1259;11031.18,-982.161;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;1261;11260.33,-1037.758;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1154;10867.66,-1168.845;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;372;11851.63,-1593.227;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;374;11853.6,-1335.992;Inherit;False;Property;_ParticleScale;ParticleScale;3;0;Create;True;0;0;0;False;0;False;0.3;0.057;0;0.3;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;371;12097.34,-1465.826;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;563;12439.73,-1362.278;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;562;12199.35,-1323.741;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TransformPositionNode;373;12247.38,-1610.452;Inherit;True;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;1089;4183.879,-1717.497;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;639;3596.267,-1477.15;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;1073;3867.631,-1569.554;Inherit;False;Random Range;-1;;145;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1082;4124.743,-1432.031;Inherit;False;randomViaYnew;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;566;4127.619,-1596.068;Inherit;False;RandomViaX;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
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
WireConnection;659;0;658;0
WireConnection;659;1;657;0
WireConnection;803;0;804;0
WireConnection;803;1;800;0
WireConnection;804;0;813;0
WireConnection;804;1;809;0
WireConnection;805;0;806;0
WireConnection;805;1;839;0
WireConnection;808;0;810;0
WireConnection;808;1;817;0
WireConnection;810;0;812;0
WireConnection;810;1;811;0
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
WireConnection;660;1;659;0
WireConnection;883;0;884;0
WireConnection;883;1;884;1
WireConnection;883;2;888;0
WireConnection;889;0;650;0
WireConnection;888;0;881;0
WireConnection;888;1;893;0
WireConnection;890;0;889;0
WireConnection;653;0;643;0
WireConnection;604;0;599;0
WireConnection;604;1;597;0
WireConnection;605;0;614;0
WireConnection;605;1;606;0
WireConnection;606;0;604;0
WireConnection;606;1;597;0
WireConnection;674;0;675;0
WireConnection;674;1;673;0
WireConnection;673;0;675;0
WireConnection;673;1;672;0
WireConnection;675;0;683;0
WireConnection;675;1;671;0
WireConnection;687;0;669;0
WireConnection;687;1;688;0
WireConnection;666;0;674;0
WireConnection;775;0;774;0
WireConnection;775;1;680;0
WireConnection;671;1;775;0
WireConnection;774;0;772;0
WireConnection;774;1;773;0
WireConnection;665;0;687;0
WireConnection;599;0;669;0
WireConnection;599;1;601;0
WireConnection;669;0;668;0
WireConnection;669;1;666;0
WireConnection;669;2;670;0
WireConnection;669;3;688;0
WireConnection;801;0;804;0
WireConnection;801;1;803;0
WireConnection;652;0;636;0
WireConnection;650;0;652;0
WireConnection;645;0;644;0
WireConnection;645;1;635;0
WireConnection;884;0;645;0
WireConnection;879;0;883;0
WireConnection;879;1;632;0
WireConnection;638;0;650;0
WireConnection;638;1;651;0
WireConnection;651;0;653;0
WireConnection;644;0;638;0
WireConnection;880;0;879;0
WireConnection;545;0;541;0
WireConnection;654;1;639;3
WireConnection;656;0;654;0
WireConnection;1085;1;639;2
WireConnection;893;1;636;0
WireConnection;809;1;808;0
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
WireConnection;785;3;1086;0
WireConnection;791;0;779;0
WireConnection;792;0;795;0
WireConnection;792;1;797;0
WireConnection;793;1;792;0
WireConnection;795;0;794;0
WireConnection;795;1;796;0
WireConnection;818;0;806;0
WireConnection;790;0;785;0
WireConnection;1070;28;129;0
WireConnection;1070;9;1204;0
WireConnection;1070;11;1074;0
WireConnection;1088;28;218;0
WireConnection;1088;9;1090;0
WireConnection;1088;11;1089;0
WireConnection;219;0;1088;0
WireConnection;1083;0;194;0
WireConnection;1083;1;215;0
WireConnection;806;0;816;0
WireConnection;806;1;807;0
WireConnection;806;2;815;0
WireConnection;806;3;839;0
WireConnection;807;0;801;0
WireConnection;1104;0;1098;0
WireConnection;1104;1;1125;0
WireConnection;1109;0;1107;0
WireConnection;1109;1;1126;0
WireConnection;1118;0;1117;0
WireConnection;1157;0;129;0
WireConnection;264;0;194;0
WireConnection;1136;0;1135;0
WireConnection;1136;2;1138;0
WireConnection;1135;0;1132;0
WireConnection;1135;1;1133;0
WireConnection;1138;0;1137;0
WireConnection;1138;1;1133;0
WireConnection;1110;1;1128;0
WireConnection;1110;2;1114;0
WireConnection;1110;3;1106;0
WireConnection;1160;0;1136;0
WireConnection;1160;1;1099;0
WireConnection;1117;0;1076;0
WireConnection;1117;2;1116;0
WireConnection;1123;0;1076;0
WireConnection;1123;1;1116;0
WireConnection;1185;0;1076;0
WireConnection;1185;1;1184;0
WireConnection;1124;0;1123;0
WireConnection;1186;0;1185;0
WireConnection;1119;0;1118;0
WireConnection;1200;0;1186;0
WireConnection;1200;1;1199;0
WireConnection;1204;0;1203;1
WireConnection;1204;1;1203;2
WireConnection;1204;2;129;0
WireConnection;1075;0;1070;0
WireConnection;1187;0;1186;0
WireConnection;1093;1;1104;0
WireConnection;1093;2;1094;0
WireConnection;1093;3;1166;0
WireConnection;1106;1;1109;0
WireConnection;1106;2;1115;0
WireConnection;1106;3;1093;0
WireConnection;1128;0;1127;0
WireConnection;1128;1;1130;0
WireConnection;1120;0;1124;0
WireConnection;1;2;1249;0
WireConnection;1;5;26;0
WireConnection;26;0;589;0
WireConnection;589;0;1045;0
WireConnection;589;1;563;0
WireConnection;1045;0;1192;0
WireConnection;1045;1;1013;0
WireConnection;1045;2;1046;0
WireConnection;1167;1;1168;0
WireConnection;1167;2;1170;0
WireConnection;1167;3;1160;0
WireConnection;1177;0;1180;0
WireConnection;1177;1;1173;0
WireConnection;1177;2;1179;0
WireConnection;1177;3;1145;0
WireConnection;1178;0;1161;0
WireConnection;1179;0;1178;0
WireConnection;1180;0;1176;0
WireConnection;1172;0;1167;0
WireConnection;1172;1;1177;0
WireConnection;1158;0;872;0
WireConnection;1158;1;1172;0
WireConnection;1158;2;1161;0
WireConnection;1197;0;1190;0
WireConnection;1197;1;1191;0
WireConnection;1197;2;1195;0
WireConnection;1194;0;1188;0
WireConnection;1194;1;1191;0
WireConnection;1194;2;1195;0
WireConnection;1163;1;1194;0
WireConnection;1163;2;1159;0
WireConnection;1163;3;1233;0
WireConnection;1162;0;1158;0
WireConnection;1176;0;1161;0
WireConnection;1176;2;1181;0
WireConnection;1168;0;1161;0
WireConnection;1168;1;1169;0
WireConnection;1146;0;1095;0
WireConnection;1146;1;1145;0
WireConnection;1146;2;1207;0
WireConnection;1145;0;1141;0
WireConnection;1099;0;1100;0
WireConnection;1099;1;1097;0
WireConnection;1099;2;1100;2
WireConnection;1100;0;1096;0
WireConnection;1097;0;1100;1
WireConnection;1097;1;1146;0
WireConnection;1230;0;1228;0
WireConnection;1230;1;1231;0
WireConnection;1230;2;1237;0
WireConnection;1196;1;1197;0
WireConnection;1196;2;1159;0
WireConnection;1196;3;1163;0
WireConnection;1233;0;1234;0
WireConnection;1233;1;1235;0
WireConnection;1233;2;1234;2
WireConnection;1235;0;1234;1
WireConnection;1235;1;1230;0
WireConnection;1234;0;1236;0
WireConnection;1231;0;1232;0
WireConnection;1229;0;1216;0
WireConnection;1229;1;1217;0
WireConnection;1206;0;1229;0
WireConnection;1207;0;1206;0
WireConnection;1241;0;1207;0
WireConnection;1249;0;10;0
WireConnection;1249;1;1248;0
WireConnection;1249;2;1247;0
WireConnection;1250;0;1245;0
WireConnection;1251;0;1246;0
WireConnection;1242;0;1244;0
WireConnection;1242;1;1243;0
WireConnection;1247;0;1254;0
WireConnection;1247;1;1242;0
WireConnection;1254;0;1251;0
WireConnection;1254;1;1255;0
WireConnection;1256;0;1263;0
WireConnection;1256;1;1222;0
WireConnection;1223;0;1222;0
WireConnection;1223;1;1205;0
WireConnection;1225;0;1223;0
WireConnection;1216;0;1205;0
WireConnection;1216;1;1215;0
WireConnection;1216;2;1209;0
WireConnection;1257;0;1256;0
WireConnection;1222;0;1210;0
WireConnection;1210;0;1205;0
WireConnection;1210;1;1209;0
WireConnection;1262;0;1210;0
WireConnection;1263;0;1210;0
WireConnection;1240;1;1239;0
WireConnection;1213;0;1211;0
WireConnection;1213;1;1193;0
WireConnection;1239;0;1110;0
WireConnection;1239;1;1154;0
WireConnection;1239;2;1213;0
WireConnection;1192;0;1110;0
WireConnection;1192;1;1196;0
WireConnection;1192;2;1213;0
WireConnection;1264;0;1154;0
WireConnection;1264;1;1196;0
WireConnection;1264;2;1261;0
WireConnection;1259;0;1258;0
WireConnection;1259;2;1238;0
WireConnection;1261;0;1259;0
WireConnection;371;0;372;0
WireConnection;371;1;374;0
WireConnection;563;0;373;0
WireConnection;563;1;562;0
WireConnection;373;0;371;0
WireConnection;1073;1;639;1
WireConnection;1082;0;1085;0
WireConnection;566;0;1073;0
ASEEND*/
//CHKSM=F5FC864CC5D0A21E4AB60AF3BE602786AC08CC2B