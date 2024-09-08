// Made with Amplify Shader Editor v1.9.2.1
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Potato Simple"
{
	Properties
	{
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		_TransformationStagger("TransformationStagger", Range( 0 , 1)) = 0
		_Transformation("Transformation", Range( 0 , 1)) = 0
		_ParticleScale("ParticleScale", Range( 0 , 0.3)) = 0.3
		_RotateY_Speed("RotateY_Speed", Range( 0 , 500)) = 0
		_RotateY_Variance("RotateY_Variance", Range( 0 , 10)) = 10
		_RotateZ_Speed("RotateZ_Speed", Range( 0 , 500)) = 0
		_RotateZ_Variance("RotateZ_Variance", Range( 0 , 10)) = 10
		_FusionPointRingOffsetStrength("FusionPointRingOffsetStrength", Range( 0 , 1)) = 0.0434969
		_FusionPointOnAxisOffset("FusionPointOnAxisOffset", Range( 0 , 1)) = 0.0434969
		_TransformationPosition("TransformationPosition", Vector) = (0,0,0,0)
		_Collection("Collection", Range( 0 , 1)) = 0
		_CollectionStagger("CollectionStagger", Range( 0 , 1)) = 0
		_CollectionPoint("CollectionPoint", Vector) = (0,0,0,0)


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

		Cull Front
		AlphaToMask Off

		

		HLSLINCLUDE
		#pragma target 5.0
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
			ZTest Less
			Offset 0.38 , 0.19
			ColorMask RGBA

			

			HLSLPROGRAM

			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define shader_feature_local _RECEIVE_SHADOWS_OFF
			#define ASE_SRP_VERSION 140009
			#define ASE_USING_SAMPLING_MACROS 1


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
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float _RotateY_Speed;
			float _RotateY_Variance;
			float _RotateZ_Speed;
			float _RotateZ_Variance;
			float _FusionPointRingOffsetStrength;
			float _FusionPointOnAxisOffset;
			float _Transformation;
			float _TransformationStagger;
			float _Collection;
			float _CollectionStagger;
			float _ParticleScale;
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
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + (0) ) * _RotateY_Variance ) ) );
				float3 TransformationPosition264 = _TransformationPosition;
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaZ656 ) * _RotateZ_Variance ) ) );
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_1 = (transform639.x).xx;
				float dotResult4_g138 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g138 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g138 ) * 43758.55 ) ));
				float temp_output_1073_0 = lerpResult10_g138;
				float randomViaYnew1082 = temp_output_1073_0;
				float2 appendResult638 = (float2(temp_output_650_0 , ( ( randomViaYnew1082 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength );
				float2 temp_cast_2 = (RandomViaZ656).xx;
				float dotResult4_g140 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g140 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g140 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g140 ) , 0.0));
				float4 TransformationPositionWithRingOffset880 = ( appendResult883 + float4( TransformationPosition264 , 0.0 ) );
				float3 rotatedValue806 = RotateAroundAxis( TransformationPosition264, TransformationPositionWithRingOffset880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float3 RotatedZAroundFusionPointOffset818 = rotatedValue806;
				float3 rotatedValue785 = RotateAroundAxis( TransformationPosition264, RotatedZAroundFusionPointOffset818, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float3 RotatedYAroundClosesAxisPointOffset790 = rotatedValue785;
				float temp_output_25_0_g141 = saturate( _TransformationStagger );
				float RandomViaX566 = temp_output_1073_0;
				float2 temp_cast_6 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_6 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float4 lerpResult982 = lerp( transform872 , float4( RotatedYAroundClosesAxisPointOffset790 , 0.0 ) , TransformationStaggered1075);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_8 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_8 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
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

				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = float3( 0.5, 0.5, 0.5 );
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
			#define ASE_USING_SAMPLING_MACROS 1


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
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float _RotateY_Speed;
			float _RotateY_Variance;
			float _RotateZ_Speed;
			float _RotateZ_Variance;
			float _FusionPointRingOffsetStrength;
			float _FusionPointOnAxisOffset;
			float _Transformation;
			float _TransformationStagger;
			float _Collection;
			float _CollectionStagger;
			float _ParticleScale;
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
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + (0) ) * _RotateY_Variance ) ) );
				float3 TransformationPosition264 = _TransformationPosition;
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaZ656 ) * _RotateZ_Variance ) ) );
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_1 = (transform639.x).xx;
				float dotResult4_g138 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g138 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g138 ) * 43758.55 ) ));
				float temp_output_1073_0 = lerpResult10_g138;
				float randomViaYnew1082 = temp_output_1073_0;
				float2 appendResult638 = (float2(temp_output_650_0 , ( ( randomViaYnew1082 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength );
				float2 temp_cast_2 = (RandomViaZ656).xx;
				float dotResult4_g140 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g140 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g140 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g140 ) , 0.0));
				float4 TransformationPositionWithRingOffset880 = ( appendResult883 + float4( TransformationPosition264 , 0.0 ) );
				float3 rotatedValue806 = RotateAroundAxis( TransformationPosition264, TransformationPositionWithRingOffset880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float3 RotatedZAroundFusionPointOffset818 = rotatedValue806;
				float3 rotatedValue785 = RotateAroundAxis( TransformationPosition264, RotatedZAroundFusionPointOffset818, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float3 RotatedYAroundClosesAxisPointOffset790 = rotatedValue785;
				float temp_output_25_0_g141 = saturate( _TransformationStagger );
				float RandomViaX566 = temp_output_1073_0;
				float2 temp_cast_6 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_6 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float4 lerpResult982 = lerp( transform872 , float4( RotatedYAroundClosesAxisPointOffset790 , 0.0 ) , TransformationStaggered1075);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_8 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_8 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
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
			#define ASE_USING_SAMPLING_MACROS 1


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
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float _RotateY_Speed;
			float _RotateY_Variance;
			float _RotateZ_Speed;
			float _RotateZ_Variance;
			float _FusionPointRingOffsetStrength;
			float _FusionPointOnAxisOffset;
			float _Transformation;
			float _TransformationStagger;
			float _Collection;
			float _CollectionStagger;
			float _ParticleScale;
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
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + (0) ) * _RotateY_Variance ) ) );
				float3 TransformationPosition264 = _TransformationPosition;
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaZ656 ) * _RotateZ_Variance ) ) );
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_1 = (transform639.x).xx;
				float dotResult4_g138 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g138 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g138 ) * 43758.55 ) ));
				float temp_output_1073_0 = lerpResult10_g138;
				float randomViaYnew1082 = temp_output_1073_0;
				float2 appendResult638 = (float2(temp_output_650_0 , ( ( randomViaYnew1082 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength );
				float2 temp_cast_2 = (RandomViaZ656).xx;
				float dotResult4_g140 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g140 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g140 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g140 ) , 0.0));
				float4 TransformationPositionWithRingOffset880 = ( appendResult883 + float4( TransformationPosition264 , 0.0 ) );
				float3 rotatedValue806 = RotateAroundAxis( TransformationPosition264, TransformationPositionWithRingOffset880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float3 RotatedZAroundFusionPointOffset818 = rotatedValue806;
				float3 rotatedValue785 = RotateAroundAxis( TransformationPosition264, RotatedZAroundFusionPointOffset818, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float3 RotatedYAroundClosesAxisPointOffset790 = rotatedValue785;
				float temp_output_25_0_g141 = saturate( _TransformationStagger );
				float RandomViaX566 = temp_output_1073_0;
				float2 temp_cast_6 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_6 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float4 lerpResult982 = lerp( transform872 , float4( RotatedYAroundClosesAxisPointOffset790 , 0.0 ) , TransformationStaggered1075);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_8 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_8 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
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
			#define ASE_USING_SAMPLING_MACROS 1


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
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float _RotateY_Speed;
			float _RotateY_Variance;
			float _RotateZ_Speed;
			float _RotateZ_Variance;
			float _FusionPointRingOffsetStrength;
			float _FusionPointOnAxisOffset;
			float _Transformation;
			float _TransformationStagger;
			float _Collection;
			float _CollectionStagger;
			float _ParticleScale;
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
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + (0) ) * _RotateY_Variance ) ) );
				float3 TransformationPosition264 = _TransformationPosition;
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaZ656 ) * _RotateZ_Variance ) ) );
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_1 = (transform639.x).xx;
				float dotResult4_g138 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g138 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g138 ) * 43758.55 ) ));
				float temp_output_1073_0 = lerpResult10_g138;
				float randomViaYnew1082 = temp_output_1073_0;
				float2 appendResult638 = (float2(temp_output_650_0 , ( ( randomViaYnew1082 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength );
				float2 temp_cast_2 = (RandomViaZ656).xx;
				float dotResult4_g140 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g140 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g140 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g140 ) , 0.0));
				float4 TransformationPositionWithRingOffset880 = ( appendResult883 + float4( TransformationPosition264 , 0.0 ) );
				float3 rotatedValue806 = RotateAroundAxis( TransformationPosition264, TransformationPositionWithRingOffset880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float3 RotatedZAroundFusionPointOffset818 = rotatedValue806;
				float3 rotatedValue785 = RotateAroundAxis( TransformationPosition264, RotatedZAroundFusionPointOffset818, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float3 RotatedYAroundClosesAxisPointOffset790 = rotatedValue785;
				float temp_output_25_0_g141 = saturate( _TransformationStagger );
				float RandomViaX566 = temp_output_1073_0;
				float2 temp_cast_6 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_6 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float4 lerpResult982 = lerp( transform872 , float4( RotatedYAroundClosesAxisPointOffset790 , 0.0 ) , TransformationStaggered1075);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_8 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_8 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
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
			#define shader_feature_local _RECEIVE_SHADOWS_OFF
			#define ASE_SRP_VERSION 140009
			#define ASE_USING_SAMPLING_MACROS 1


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
			float3 _TransformationPosition;
			float3 _CollectionPoint;
			float _RotateY_Speed;
			float _RotateY_Variance;
			float _RotateZ_Speed;
			float _RotateZ_Variance;
			float _FusionPointRingOffsetStrength;
			float _FusionPointOnAxisOffset;
			float _Transformation;
			float _TransformationStagger;
			float _Collection;
			float _CollectionStagger;
			float _ParticleScale;
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
				float3 appendResult788 = (float3(0.0 , 1.0 , 0.0));
				float4 transform639 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform639.z).xx;
				float dotResult4_g134 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g134 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g134 ) * 43758.55 ) ));
				float RandomViaZ656 = lerpResult10_g134;
				float temp_output_783_0 = ( _RotateY_Speed * ( 1.0 + ( ( RandomViaZ656 + (0) ) * _RotateY_Variance ) ) );
				float3 TransformationPosition264 = _TransformationPosition;
				float3 appendResult816 = (float3(0.0 , 0.0 , 1.0));
				float temp_output_804_0 = ( _RotateZ_Speed * ( 1.0 + ( ( RandomViaZ656 + RandomViaZ656 ) * _RotateZ_Variance ) ) );
				float temp_output_650_0 = ( ( RandomViaZ656 - 0.5 ) * 2.0 );
				float2 temp_cast_1 = (transform639.x).xx;
				float dotResult4_g138 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g138 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g138 ) * 43758.55 ) ));
				float temp_output_1073_0 = lerpResult10_g138;
				float randomViaYnew1082 = temp_output_1073_0;
				float2 appendResult638 = (float2(temp_output_650_0 , ( ( randomViaYnew1082 - 0.5 ) * 2.0 )));
				float2 normalizeResult644 = ASESafeNormalize( appendResult638 );
				float2 break884 = ( normalizeResult644 * _FusionPointRingOffsetStrength );
				float2 temp_cast_2 = (RandomViaZ656).xx;
				float dotResult4_g140 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g140 = lerp( -1.0 , 1.0 , frac( ( sin( dotResult4_g140 ) * 43758.55 ) ));
				float4 appendResult883 = (float4(break884.x , break884.y , ( _FusionPointOnAxisOffset * lerpResult10_g140 ) , 0.0));
				float4 TransformationPositionWithRingOffset880 = ( appendResult883 + float4( TransformationPosition264 , 0.0 ) );
				float3 rotatedValue806 = RotateAroundAxis( TransformationPosition264, TransformationPositionWithRingOffset880.xyz, normalize( appendResult816 ), radians( ( temp_output_804_0 + ( temp_output_804_0 * _TimeParameters.x ) ) ) );
				float3 RotatedZAroundFusionPointOffset818 = rotatedValue806;
				float3 rotatedValue785 = RotateAroundAxis( TransformationPosition264, RotatedZAroundFusionPointOffset818, normalize( appendResult788 ), radians( ( temp_output_783_0 + ( temp_output_783_0 * _TimeParameters.x ) ) ) );
				float3 RotatedYAroundClosesAxisPointOffset790 = rotatedValue785;
				float temp_output_25_0_g141 = saturate( _TransformationStagger );
				float RandomViaX566 = temp_output_1073_0;
				float2 temp_cast_6 = (RandomViaX566).xx;
				float dotResult4_g142 = dot( temp_cast_6 , float2( 12.9898,78.233 ) );
				float lerpResult10_g142 = lerp( 0.0 , temp_output_25_0_g141 , frac( ( sin( dotResult4_g142 ) * 43758.55 ) ));
				float temp_output_4_0_g141 = lerpResult10_g142;
				float TransformationStaggered1075 = saturate( (0.0 + (_Transformation - temp_output_4_0_g141) * (1.0 - 0.0) / (( temp_output_4_0_g141 + ( 1.0 - temp_output_25_0_g141 ) ) - temp_output_4_0_g141)) );
				float4 lerpResult982 = lerp( transform872 , float4( RotatedYAroundClosesAxisPointOffset790 , 0.0 ) , TransformationStaggered1075);
				float temp_output_25_0_g143 = saturate( _CollectionStagger );
				float2 temp_cast_8 = (RandomViaX566).xx;
				float dotResult4_g144 = dot( temp_cast_8 , float2( 12.9898,78.233 ) );
				float lerpResult10_g144 = lerp( 0.0 , temp_output_25_0_g143 , frac( ( sin( dotResult4_g144 ) * 43758.55 ) ));
				float temp_output_4_0_g143 = lerpResult10_g144;
				float CollectionAdjusted219 = saturate( (0.0 + (_Collection - temp_output_4_0_g143) * (1.0 - 0.0) / (( temp_output_4_0_g143 + ( 1.0 - temp_output_25_0_g143 ) ) - temp_output_4_0_g143)) );
				float4 lerpResult1045 = lerp( lerpResult982 , float4( _CollectionPoint , 0.0 ) , CollectionAdjusted219);
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
Node;AmplifyShaderEditor.CommentaryNode;661;3261.751,-4423.125;Inherit;False;1515.317;661.3047;Comment;20;644;652;650;651;638;636;653;643;645;635;659;660;657;632;884;888;889;890;893;658;OffsetFromFusionAxis;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;616;1170.652,-6725.051;Inherit;False;2195.834;649.1808;Comment;8;615;614;597;601;605;606;599;604;MoveToFusionAxis;0.5263038,0.5465524,0.9528302,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;590;9421.493,-1132.497;Inherit;False;742.2939;484.9873;vertices;6;373;371;372;374;562;563;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;565;4171.773,-359.4157;Inherit;False;1983.349;986.3488;Comment;21;856;857;852;855;854;526;515;531;525;533;529;538;540;539;519;537;532;516;858;859;861;CenterColor;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;265;6124.333,-1414.632;Inherit;False;1103.841;648.4066;;4;194;215;264;1083;Move To Fusion Point;0.6653691,0.8490566,0.6127626,1;0;0
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
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;659;3961.063,-3931.692;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;663;3749.409,-7470.825;Inherit;False;1864.76;1237.109;Rotation;19;665;668;670;688;669;687;666;675;673;683;671;676;674;672;772;773;774;775;680;Rotate Around Fusion Axis Point X;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;777;6001.128,-4202.071;Inherit;False;1864.76;1237.109;Rotation;20;797;796;795;793;792;791;790;788;787;786;785;784;783;782;781;780;779;778;794;1086;Rotate Around Fusion Axis Point X;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;799;7881.625,-2858.828;Inherit;False;1864.76;1237.109;Rotation;21;818;817;816;815;813;812;811;810;809;808;807;806;805;804;803;801;839;878;877;894;800;Rotate Around Fusion Axis Point Z;0.09189212,0.3626977,0.5566038,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;803;8677.732,-2318.941;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;804;8603.021,-2663.819;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;805;8993.006,-1883.03;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RadiansOpNode;807;9096.615,-2574.726;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;808;8313.527,-2505.531;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;810;8103.583,-2567.441;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;371;9689.493,-952.4478;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;563;10031.88,-848.9;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.PosVertexDataNode;372;9443.784,-1079.849;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;374;9445.753,-822.614;Inherit;False;Property;_ParticleScale;ParticleScale;5;0;Create;True;0;0;0;False;0;False;0.3;0.057;0;0.3;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;529;5289.352,136.1183;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;852;5299.484,-78.13897;Inherit;False;Property;_FusionAxisColor_MinDistance;FusionAxisColor_MinDistance;8;0;Create;True;0;0;0;False;0;False;0;0.41;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;854;5296.541,-0.2754244;Inherit;False;Property;_FusionAxisColor_MaxDistance;FusionAxisColor_MaxDistance;9;0;Create;True;0;0;0;False;0;False;0;1.04;0;0;0;1;FLOAT;0
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
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;562;9791.503,-810.3629;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;660;4202.961,-3901.392;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;883;4707.241,-4179.629;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;881;3787.144,-4590.234;Inherit;False;Property;_FusionPointOnAxisOffset;FusionPointOnAxisOffset;21;0;Create;True;0;0;0;False;0;False;0.0434969;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;889;3986.808,-4416.03;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;888;4353.458,-4353.105;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;890;4130.204,-4414.302;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;653;3535.76,-4165.518;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.StickyNoteNode;894;9186.798,-1792.113;Inherit;False;332.6025;127.3905;New Note;;1,1,1,1;The idea here is to only calculate offset, so it's much easier to add them together. If we take the final position and add various rotations, it will keep adding the offset from the spawnpoint$The alternative would be to rotate the entire position one after another, pluggin in the result of one rotation into the next one.;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;877;8124.275,-1971.632;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;878;8234.653,-2147.604;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DotProductOpNode;604;1692.677,-6449.249;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;605;2040.339,-6484.41;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;606;1859.702,-6334.335;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;601;1243.425,-6432.498;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleTimeNode;672;4186.157,-6896.999;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;674;4746.424,-7313.814;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;683;4192.368,-7414.898;Inherit;False;Property;_RotateX_Speed;RotateX_Speed;10;0;Create;True;0;0;0;False;0;False;0;0;0;500;0;1;FLOAT;0
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
Node;AmplifyShaderEditor.RangedFloatNode;680;3771.635,-7008.538;Inherit;False;Property;_RotateX_Variance;RotateX_Variance;11;0;Create;True;0;0;0;False;0;False;10;1.2;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;676;3893.303,-6864.098;Inherit;False;545;UseTime;1;0;OBJECT;;False;1;INT;0
Node;AmplifyShaderEditor.GetLocalVarNode;670;3821.301,-6672.097;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;665;5088.636,-6760.083;Inherit;False;RotatedXAroundClosesAxisPointOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TransformPositionNode;26;10517.42,-1634.364;Inherit;False;World;Object;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;589;10315.09,-1456.795;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TransformPositionNode;373;9839.534,-1097.074;Inherit;True;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.LerpOp;523;10645.76,-2232.72;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;10;10121.57,-2399.429;Inherit;False;Property;_MainColor;MainColor;6;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,0.9284637,6.115068,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;518;10122.22,-2134.628;Inherit;False;Property;_FusionAxisColor;FusionAxisColor;7;1;[HDR];Create;True;0;0;0;False;0;False;0.2042542,0.5284038,0.9622642,0;0,3.721394,16,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;527;10379.52,-2067.89;Inherit;False;526;FusionColorAmount;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;599;1533.501,-6536.839;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;597;1414.569,-6280.794;Inherit;False;Property;_FusionDirection;FusionDirection;24;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;614;1896.167,-6665.912;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;615;2317.098,-6632.957;Inherit;False;ClosestPointOnFusionAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;669;4498.267,-6604.27;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;801;8945.199,-2577.705;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;800;8368.694,-2315.673;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;817;7970.836,-2366.45;Inherit;False;Property;_RotateZ_Variance;RotateZ_Variance;15;0;Create;True;0;0;0;False;0;False;10;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;812;7897.837,-2703.874;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;813;8265.975,-2793.132;Inherit;False;Property;_RotateZ_Speed;RotateZ_Speed;14;0;Create;True;0;0;0;False;0;False;0;229;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;652;3531.116,-4309.168;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;650;3727.319,-4311.346;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;635;4005.986,-4066.838;Inherit;False;Property;_FusionPointRingOffsetStrength;FusionPointRingOffsetStrength;20;0;Create;True;0;0;0;False;0;False;0.0434969;0.405;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;657;3594.42,-3849.71;Inherit;False;Property;_FusionPointRingOffsetVariance;FusionPointRingOffsetVariance;22;0;Create;True;0;0;0;False;0;False;0;0;0;2.17;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;645;4348.176,-4043.881;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.BreakToComponentsNode;884;4519.02,-4148.945;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SimpleAddOpNode;879;4960.855,-4039.917;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;638;3988.828,-4216.127;Inherit;False;FLOAT2;4;0;FLOAT;0.6;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;651;3738.796,-4149.131;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalizeNode;644;4173.121,-4201.443;Inherit;False;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;658;3721.357,-3947.191;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;811;7865.956,-2531.965;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;215;6243.678,-1337.513;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleSubtractOpNode;1083;6566.605,-1262.208;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector3Node;194;6244.808,-1108.862;Inherit;False;Property;_TransformationPosition;TransformationPosition;23;0;Create;False;0;0;0;False;0;False;0,0,0;0.007405923,1.85188,1.126751;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;632;4437.426,-3864.773;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;880;5269.801,-4028.406;Inherit;False;TransformationPositionWithRingOffset;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LerpOp;982;8550.938,-1401.157;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LerpOp;1045;9342.85,-1431.656;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;1084;8859.897,-1623.11;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;872;7937.927,-1583.662;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;491;3609.087,-3437.759;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;494;4077.087,-3263.759;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.NormalizeNode;495;4259.086,-3228.759;Inherit;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;497;4354.637,-3118.772;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;499;4672.283,-3201.784;Inherit;False;FLOAT3;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;496;4484.203,-3257.291;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;492;3883.087,-3352.759;Inherit;False;Random Range;-1;;111;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;-1;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;503;4182.101,-3110.515;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;493;3884.087,-3202.759;Inherit;False;Random Range;-1;;112;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;-1;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;504;3888.101,-3051.515;Inherit;False;Random Range;-1;;113;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;490;3883.103,-2914.285;Inherit;False;Property;_OffsetFromFusionPointVariation;OffsetFromFusionPointVariation;19;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;489;4138.258,-3446.202;Inherit;False;Property;_OffsetFromFusionPointStrength;OffsetFromFusionPointStrength;18;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;498;4823.553,-3167.404;Inherit;False;OffsetAroundFusionPoint;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;545;4162.063,-2115.116;Inherit;False;UseTime;-1;True;1;0;INT;0;False;1;INT;0
Node;AmplifyShaderEditor.FunctionNode;654;3888.296,-1290.296;Inherit;False;Random Range;-1;;134;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;656;4125.483,-1270.315;Inherit;False;RandomViaZ;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;566;4126.319,-1596.068;Inherit;False;RandomViaX;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;541;3939.136,-2126.692;Inherit;False;Property;_UseTime;UseTime;0;0;Create;True;0;0;0;False;0;False;0;0;False;0;1;INT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1082;4124.743,-1432.031;Inherit;False;randomViaYnew;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;1073;3892.331,-1552.654;Inherit;False;Random Range;-1;;138;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;1085;3888.474,-1430.683;Inherit;False;Random Range;-1;;139;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;639;3622.267,-1468.05;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;893;3718.189,-4434.687;Inherit;False;Random Range;-1;;140;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;-1;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;636;3319.022,-4372.87;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;643;3283.913,-4156.916;Inherit;False;1082;randomViaYnew;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;839;8163.858,-1788.351;Inherit;False;880;TransformationPositionWithRingOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;815;7945.041,-2103.994;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;1076;8244.706,-1156.088;Inherit;False;1075;TransformationStaggered;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1081;7779.578,-1362.719;Inherit;False;880;TransformationPositionWithRingOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;806;8741.036,-2111.354;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;816;8550.547,-2230.927;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;809;8464.257,-2608.494;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
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
Node;AmplifyShaderEditor.GetLocalVarNode;796;6010.458,-3823.208;Inherit;False;-1;;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;794;6268.696,-3829.847;Inherit;False;656;RandomViaZ;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;781;6444.087,-4146.144;Inherit;False;Property;_RotateY_Speed;RotateY_Speed;12;0;Create;True;0;0;0;False;0;False;0;249;0;500;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;797;6023.354,-3741.79;Inherit;False;Property;_RotateY_Variance;RotateY_Variance;13;0;Create;True;0;0;0;False;0;False;10;0.05;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;788;6357.445,-3491.246;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;818;9179.871,-2124;Inherit;False;RotatedZAroundFusionPointOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;786;6122.927,-3214.935;Inherit;False;880;TransformationPositionWithRingOffset;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;787;6278.381,-3325.129;Inherit;False;264;TransformationPosition;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;790;7300.373,-3468.726;Inherit;False;RotatedYAroundClosesAxisPointOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;1086;6159.972,-3087.017;Inherit;False;818;RotatedZAroundFusionPointOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;1087;8079.951,-1274.375;Inherit;False;790;RotatedYAroundClosesAxisPointOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;264;6897.256,-1124.754;Inherit;False;TransformationPosition;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;1070;4215.06,-998.3702;Inherit;False;Stagger;-1;;141;93a439cb4f13e644e8dcf460c2df1f83;0;3;28;FLOAT;0;False;9;FLOAT;0;False;11;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1074;3931.98,-885.5711;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;1088;4455.288,-1805.048;Inherit;False;Stagger;-1;;143;93a439cb4f13e644e8dcf460c2df1f83;0;3;28;FLOAT;0;False;9;FLOAT;0;False;11;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1089;4183.879,-1717.497;Inherit;False;566;RandomViaX;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;218;3925.724,-1876.804;Inherit;False;Property;_Collection;Collection;25;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1090;3896.708,-1787.538;Inherit;False;Property;_CollectionStagger;CollectionStagger;26;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;219;4752.999,-1792.74;Inherit;False;CollectionAdjusted;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1068;3654.746,-818.077;Inherit;False;Property;_Spawn;Spawn;2;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;129;3757.641,-1153.977;Inherit;False;Property;_Transformation;Transformation;4;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1091;3768.596,-1067.729;Inherit;False;Property;_TransformationStagger;TransformationStagger;3;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;1075;4496.327,-994.9201;Inherit;False;TransformationStaggered;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;1071;3766.683,-976.5091;Inherit;False;Property;_SpawnStagger;SpawnStagger;1;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;1046;9033.692,-1037.941;Inherit;False;219;CollectionAdjusted;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;1013;8943.765,-1256.788;Inherit;False;Property;_CollectionPoint;CollectionPoint;27;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1111;10800.12,-1748.484;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1112;10800.12,-1748.484;Float;False;True;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;Potato Simple;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;True;True;1;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;7;True;12;all;0;False;True;1;1;False;;0;False;;1;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;True;True;1;False;;True;1;False;;True;True;0.38;False;;0.19;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;;0;0;Standard;23;Surface;0;0;  Blend;0;0;Two Sided;0;638445459076966309;Forward Only;0;0;Cast Shadows;0;638445459200380007;  Use Shadow Threshold;0;0;Receive Shadows;0;638445459265593308;GPU Instancing;0;638445464011047957;LOD CrossFade;0;638445459625525350;Built-in Fog;0;638445459686658320;DOTS Instancing;0;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;638445459972409147;  Phong;1;638445459854471444;  Strength;0.5,False,;638445459900693259;  Type;1;638445459926645216;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Vertex Position,InvertActionOnDeselection;0;638445454851483830;0;10;False;True;False;True;False;False;True;True;True;False;False;;True;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1113;10800.12,-1748.484;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1114;10800.12,-1748.484;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1115;10800.12,-1748.484;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1116;10800.12,-1748.484;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1117;10800.12,-1748.484;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1118;10800.12,-1748.484;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1119;10800.12,-1748.484;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1120;10800.12,-1748.484;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;;0;0;Standard;0;False;0
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
WireConnection;807;0;801;0
WireConnection;808;0;810;0
WireConnection;808;1;817;0
WireConnection;810;0;812;0
WireConnection;810;1;811;0
WireConnection;371;0;372;0
WireConnection;371;1;374;0
WireConnection;563;0;373;0
WireConnection;563;1;562;0
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
WireConnection;26;0;589;0
WireConnection;589;0;1045;0
WireConnection;589;1;563;0
WireConnection;373;0;371;0
WireConnection;523;0;10;0
WireConnection;523;1;518;0
WireConnection;523;2;527;0
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
WireConnection;1083;0;194;0
WireConnection;1083;1;215;0
WireConnection;880;0;879;0
WireConnection;982;0;872;0
WireConnection;982;1;1087;0
WireConnection;982;2;1076;0
WireConnection;1045;0;982;0
WireConnection;1045;1;1013;0
WireConnection;1045;2;1046;0
WireConnection;1084;0;872;0
WireConnection;1084;1;982;0
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
WireConnection;498;0;499;0
WireConnection;545;0;541;0
WireConnection;654;1;639;3
WireConnection;656;0;654;0
WireConnection;566;0;1073;0
WireConnection;1082;0;1073;0
WireConnection;1073;1;639;1
WireConnection;1085;1;639;2
WireConnection;893;1;636;0
WireConnection;806;0;816;0
WireConnection;806;1;807;0
WireConnection;806;2;815;0
WireConnection;806;3;839;0
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
WireConnection;264;0;194;0
WireConnection;1070;28;129;0
WireConnection;1070;9;1091;0
WireConnection;1070;11;1074;0
WireConnection;1088;28;218;0
WireConnection;1088;9;1090;0
WireConnection;1088;11;1089;0
WireConnection;219;0;1088;0
WireConnection;1075;0;1070;0
WireConnection;1112;5;26;0
ASEEND*/
//CHKSM=E3763C66EEB2C7F6BC499C947CFB37386F3988F7