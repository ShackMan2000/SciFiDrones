// Made with Amplify Shader Editor v1.9.2.1
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "FusionRingVFXNew"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_Float0("Float 0", Range( 0 , 1)) = 1
		_ShowMaster("ShowMaster", Range( 0 , 1)) = 1
		_DynamicBackTecRotationSpeed("DynamicBackTecRotationSpeed", Int) = 1
		[HDR]_Color_1("Color_1", Color) = (0,0,0,0)
		[HDR]_Color_2("Color_2", Color) = (0,0,0,0)
		[HDR]_Color3("Color 3", Color) = (0,0,0,0)
		_Color_2_Start("Color_2_Start", Range( 0 , 1)) = 0.3752641
		_Color_3_Start("Color_3_Start", Range( 0 , 1)) = 0
		_Color_3_Full("Color_3_Full", Range( 0 , 1)) = 0.3752641
		_RingTexture("RingTexture", 2D) = "black" {}
		_RingTexValueRemap("RingTexValueRemap", Vector) = (0,0,0,0)
		_RingTexFixedOffset("RingTexFixedOffset", Vector) = (0,0,0,0)
		_RingTexScale("RingTexScale", Float) = 1
		_RingTexRotation("RingTexRotation", Range( -3 , 3)) = 0
		_RingSwingTex("RingSwingTex", 2D) = "white" {}
		_RingSwingSpeed("RingSwingSpeed", Range( 0 , 2)) = 0
		_RingSwingStrength("RingSwingStrength", Range( -1 , 1)) = 0
		_RingSwingExtraWobble("RingSwingExtraWobble", Range( -1 , 1)) = 0
		_RingWobbleStrength("RingWobbleStrength", Range( -1 , 1)) = 0
		_RingWobbleTex("RingWobbleTex", 2D) = "white" {}
		_RingWobbleSpeed("RingWobbleSpeed", Range( 0 , 1)) = 0
		_RingWobbleOverlayStrength("RingWobbleOverlayStrength", Range( 0 , 2)) = 0
		_RingWobbleOverlayOffset("RingWobbleOverlayOffset", Range( 0 , 1)) = 0
		_RingWobbleOverlayFrequency("RingWobbleOverlayFrequency", Range( 0 , 10)) = 0
		_RingSwingColorRangeBoost("RingSwingColorRangeBoost", Float) = 0
		_BackTexture("BackTexture", 2D) = "black" {}
		_BackTexValueRemap("BackTexValueRemap", Vector) = (0,0,0,0)
		_BackTexScale("BackTexScale", Float) = 1
		_BackTexRotationSpeed("BackTexRotationSpeed", Float) = 0
		_BackTexMoveToCenterSpeed("BackTexMoveToCenterSpeed", Float) = 0
		_BackTexColorRangeOverlayStrength("BackTexColorRangeOverlayStrength", Range( 0 , 5)) = 0
		_BackTexColorRangeOverlayOffset("BackTexColorRangeOverlayOffset", Range( 0 , 1)) = 0
		_BackTexColorRangeOverlayFrequency("BackTexColorRangeOverlayFrequency", Range( 0 , 10)) = 0
		_BackTexFixedRotation("BackTexFixedRotation", Range( 0 , 1)) = 0


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

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" "UniversalMaterialType"="Unlit" }

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

			Blend SrcAlpha OneMinusSrcAlpha, SrcAlpha OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#pragma instancing_options renderinglayer
			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SURFACE_TYPE_TRANSPARENT 1
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

			#define ASE_NEEDS_FRAG_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
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
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color_2;
			float4 _Color_1;
			float2 _BackTexValueRemap;
			float2 _RingTexFixedOffset;
			float2 _RingTexValueRemap;
			float _RingTexScale;
			float _Color_3_Full;
			float _Color_3_Start;
			float _Color_2_Start;
			float _BackTexMoveToCenterSpeed;
			float _BackTexRotationSpeed;
			int _DynamicBackTecRotationSpeed;
			float _BackTexScale;
			float _BackTexFixedRotation;
			float _BackTexColorRangeOverlayOffset;
			float _ShowMaster;
			float _BackTexColorRangeOverlayFrequency;
			float _RingSwingColorRangeBoost;
			float _RingTexRotation;
			float _RingSwingExtraWobble;
			float _RingSwingSpeed;
			float _RingSwingStrength;
			float _RingWobbleStrength;
			float _RingWobbleSpeed;
			float _RingWobbleOverlayStrength;
			float _RingWobbleOverlayOffset;
			float _RingWobbleOverlayFrequency;
			float _BackTexColorRangeOverlayStrength;
			float _Float0;
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
			sampler2D _RingWobbleTex;
			sampler2D _RingSwingTex;
			sampler2D _BackTexture;


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

				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				o.ase_texcoord4 = v.vertex;
				o.ase_texcoord3.zw = v.ase_texcoord1.xy;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

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
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;

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
				o.ase_texcoord = v.ase_texcoord;
				o.ase_texcoord1 = v.ase_texcoord1;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
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

				float2 temp_cast_0 = (_RingTexScale).xx;
				float2 appendResult342 = (float2(IN.ase_texcoord4.xyz.x , IN.ase_texcoord4.xyz.z));
				float Vertex_Distance_From_Center_Relative345 = ( length( appendResult342 ) / 0.7 );
				float WobbleRadiusMulti435 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _RingWobbleOverlayFrequency ) + ( _RingWobbleOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _RingWobbleOverlayStrength );
				float2 appendResult56 = (float2(IN.ase_texcoord4.xyz.x , IN.ase_texcoord4.xyz.z));
				float2 normalizeResult120 = normalize( appendResult56 );
				float2 break221 = normalizeResult120;
				float2 _SeamVector = float2(0,1);
				float dotResult117 = dot( normalizeResult120 , _SeamVector );
				float angle204 = (0.0 + (atan2( ( ( break221.x * 1.0 * _SeamVector.y ) - ( break221.y * _SeamVector.x ) ) , dotResult117 ) - ( -1.0 * PI )) * (1.0 - 0.0) / (PI - ( -1.0 * PI )));
				float2 appendResult425 = (float2(angle204 , ( _RingWobbleSpeed + ( _RingWobbleSpeed * _TimeParameters.x ) )));
				float4 tex2DNode424 = tex2D( _RingWobbleTex, appendResult425 );
				float2 appendResult429 = (float2(IN.ase_texcoord4.xyz.x , IN.ase_texcoord4.xyz.z));
				float2 normalizeResult430 = normalize( appendResult429 );
				float2 appendResult453 = (float2(( angle204 + ( _RingSwingSpeed + ( _RingSwingSpeed * _TimeParameters.x ) ) ) , ( _RingSwingExtraWobble * _TimeParameters.x )));
				float4 tex2DNode452 = tex2D( _RingSwingTex, appendResult453 );
				float temp_output_459_0 = ( _RingSwingStrength * ( ( tex2DNode452.r + tex2DNode452.g + tex2DNode452.b ) / 3.0 ) );
				float temp_output_589_0 = ( WobbleRadiusMulti435 * temp_output_459_0 );
				float2 Radial_Wobble_Offset432 = ( ( ( WobbleRadiusMulti435 * ( ( ( tex2DNode424.r + tex2DNode424.g + tex2DNode424.b ) / 3.0 ) * _RingWobbleStrength ) ) * normalizeResult430 ) + ( normalizeResult430 * temp_output_589_0 ) );
				float2 texCoord319 = IN.ase_texcoord3.xy * temp_cast_0 + ( ( ( _RingTexScale * -0.5 ) + 0.5 ) + Radial_Wobble_Offset432 + _RingTexFixedOffset );
				float cos320 = cos( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float sin320 = sin( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float2 rotator320 = mul( texCoord319 - float2( 0.5,0.5 ) , float2x2( cos320 , -sin320 , sin320 , cos320 )) + float2( 0.5,0.5 );
				float4 break483 = tex2D( _RingTexture, rotator320 );
				float temp_output_533_0 = (0.0 + (( ( break483.r + break483.g + break483.b ) / 3.0 ) - _RingTexValueRemap.x) * (1.0 - 0.0) / (_RingTexValueRemap.y - _RingTexValueRemap.x));
				float RadialSwingColorRangeBoost573 = ( ( abs( temp_output_589_0 ) + abs( temp_output_459_0 ) ) * _RingSwingColorRangeBoost );
				float BackTexColorRangeOverlay516 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _BackTexColorRangeOverlayFrequency ) + ( _BackTexColorRangeOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _BackTexColorRangeOverlayStrength );
				float2 temp_cast_1 = (_BackTexScale).xx;
				float mulTime379 = _TimeParameters.x * (float)_DynamicBackTecRotationSpeed;
				float2 appendResult377 = (float2(( ( mulTime379 * _BackTexRotationSpeed ) + _BackTexRotationSpeed ) , ( _BackTexMoveToCenterSpeed + ( _BackTexMoveToCenterSpeed * mulTime379 ) )));
				float2 texCoord378 = IN.ase_texcoord3.zw * temp_cast_1 + ( ( ( _BackTexScale * -0.5 ) + 0.5 ) + appendResult377 );
				float3 rotatedValue580 = RotateAroundAxis( float3( 0.5,0.5,0.5 ), float3( texCoord378 ,  0.0 ), normalize( float3( 0,0,1 ) ), ( ( _BackTexFixedRotation * PI ) * 0.5 ) );
				float4 tex2DNode38 = tex2D( _BackTexture, rotatedValue580.xy );
				float BackTextureColorRange369 = saturate( (0.0 + (( ( tex2DNode38.r + tex2DNode38.g + tex2DNode38.b ) / 3.0 ) - _BackTexValueRemap.x) * (1.0 - 0.0) / (_BackTexValueRemap.y - _BackTexValueRemap.x)) );
				float ColorRangeFinal543 = saturate( ( saturate( ( temp_output_533_0 + ( temp_output_533_0 * RadialSwingColorRangeBoost573 ) ) ) + saturate( ( BackTexColorRangeOverlay516 * BackTextureColorRange369 ) ) ) );
				float4 lerpResult547 = lerp( _Color_1 , _Color_2 , saturate( (0.0 + (ColorRangeFinal543 - _Color_2_Start) * (1.0 - 0.0) / (_Color_3_Start - _Color_2_Start)) ));
				float4 lerpResult550 = lerp( _Color_2 , _Color3 , saturate( (0.0 + (ColorRangeFinal543 - _Color_3_Start) * (1.0 - 0.0) / (_Color_3_Full - _Color_3_Start)) ));
				float4 temp_output_404_0 = ( ( ColorRangeFinal543 < _Color_2_Start ? _Color_1 : ( ColorRangeFinal543 < _Color_3_Start ? lerpResult547 : ( ColorRangeFinal543 < _Color_3_Full ? lerpResult550 : _Color3 ) ) ) * _ShowMaster );
				
				float dotResult590 = dot( temp_output_404_0 , float4( 1,1,1,0 ) );
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = temp_output_404_0.rgb;
				float Alpha = ( dotResult590 >= _Float0 ? 1.0 : 0.0 );
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
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 140009


			#pragma vertex vert
			#pragma fragment frag

			#pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW

			#define SHADERPASS SHADERPASS_SHADOWCASTER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"

			#define ASE_NEEDS_FRAG_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
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
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color_2;
			float4 _Color_1;
			float2 _BackTexValueRemap;
			float2 _RingTexFixedOffset;
			float2 _RingTexValueRemap;
			float _RingTexScale;
			float _Color_3_Full;
			float _Color_3_Start;
			float _Color_2_Start;
			float _BackTexMoveToCenterSpeed;
			float _BackTexRotationSpeed;
			int _DynamicBackTecRotationSpeed;
			float _BackTexScale;
			float _BackTexFixedRotation;
			float _BackTexColorRangeOverlayOffset;
			float _ShowMaster;
			float _BackTexColorRangeOverlayFrequency;
			float _RingSwingColorRangeBoost;
			float _RingTexRotation;
			float _RingSwingExtraWobble;
			float _RingSwingSpeed;
			float _RingSwingStrength;
			float _RingWobbleStrength;
			float _RingWobbleSpeed;
			float _RingWobbleOverlayStrength;
			float _RingWobbleOverlayOffset;
			float _RingWobbleOverlayFrequency;
			float _BackTexColorRangeOverlayStrength;
			float _Float0;
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
			sampler2D _RingWobbleTex;
			sampler2D _RingSwingTex;
			sampler2D _BackTexture;


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
			

			float3 _LightDirection;
			float3 _LightPosition;

			VertexOutput VertexFunction( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_texcoord3 = v.vertex;
				o.ase_texcoord2.zw = v.ase_texcoord1.xy;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

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

				float3 normalWS = TransformObjectToWorldDir( v.ase_normal );

				#if _CASTING_PUNCTUAL_LIGHT_SHADOW
					float3 lightDirectionWS = normalize(_LightPosition - positionWS);
				#else
					float3 lightDirectionWS = _LightDirection;
				#endif

				float4 clipPos = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));

				#if UNITY_REVERSED_Z
					clipPos.z = min(clipPos.z, UNITY_NEAR_CLIP_VALUE);
				#else
					clipPos.z = max(clipPos.z, UNITY_NEAR_CLIP_VALUE);
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				o.clipPos = clipPos;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;

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
				o.ase_texcoord = v.ase_texcoord;
				o.ase_texcoord1 = v.ase_texcoord1;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
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

				float2 temp_cast_0 = (_RingTexScale).xx;
				float2 appendResult342 = (float2(IN.ase_texcoord3.xyz.x , IN.ase_texcoord3.xyz.z));
				float Vertex_Distance_From_Center_Relative345 = ( length( appendResult342 ) / 0.7 );
				float WobbleRadiusMulti435 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _RingWobbleOverlayFrequency ) + ( _RingWobbleOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _RingWobbleOverlayStrength );
				float2 appendResult56 = (float2(IN.ase_texcoord3.xyz.x , IN.ase_texcoord3.xyz.z));
				float2 normalizeResult120 = normalize( appendResult56 );
				float2 break221 = normalizeResult120;
				float2 _SeamVector = float2(0,1);
				float dotResult117 = dot( normalizeResult120 , _SeamVector );
				float angle204 = (0.0 + (atan2( ( ( break221.x * 1.0 * _SeamVector.y ) - ( break221.y * _SeamVector.x ) ) , dotResult117 ) - ( -1.0 * PI )) * (1.0 - 0.0) / (PI - ( -1.0 * PI )));
				float2 appendResult425 = (float2(angle204 , ( _RingWobbleSpeed + ( _RingWobbleSpeed * _TimeParameters.x ) )));
				float4 tex2DNode424 = tex2D( _RingWobbleTex, appendResult425 );
				float2 appendResult429 = (float2(IN.ase_texcoord3.xyz.x , IN.ase_texcoord3.xyz.z));
				float2 normalizeResult430 = normalize( appendResult429 );
				float2 appendResult453 = (float2(( angle204 + ( _RingSwingSpeed + ( _RingSwingSpeed * _TimeParameters.x ) ) ) , ( _RingSwingExtraWobble * _TimeParameters.x )));
				float4 tex2DNode452 = tex2D( _RingSwingTex, appendResult453 );
				float temp_output_459_0 = ( _RingSwingStrength * ( ( tex2DNode452.r + tex2DNode452.g + tex2DNode452.b ) / 3.0 ) );
				float temp_output_589_0 = ( WobbleRadiusMulti435 * temp_output_459_0 );
				float2 Radial_Wobble_Offset432 = ( ( ( WobbleRadiusMulti435 * ( ( ( tex2DNode424.r + tex2DNode424.g + tex2DNode424.b ) / 3.0 ) * _RingWobbleStrength ) ) * normalizeResult430 ) + ( normalizeResult430 * temp_output_589_0 ) );
				float2 texCoord319 = IN.ase_texcoord2.xy * temp_cast_0 + ( ( ( _RingTexScale * -0.5 ) + 0.5 ) + Radial_Wobble_Offset432 + _RingTexFixedOffset );
				float cos320 = cos( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float sin320 = sin( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float2 rotator320 = mul( texCoord319 - float2( 0.5,0.5 ) , float2x2( cos320 , -sin320 , sin320 , cos320 )) + float2( 0.5,0.5 );
				float4 break483 = tex2D( _RingTexture, rotator320 );
				float temp_output_533_0 = (0.0 + (( ( break483.r + break483.g + break483.b ) / 3.0 ) - _RingTexValueRemap.x) * (1.0 - 0.0) / (_RingTexValueRemap.y - _RingTexValueRemap.x));
				float RadialSwingColorRangeBoost573 = ( ( abs( temp_output_589_0 ) + abs( temp_output_459_0 ) ) * _RingSwingColorRangeBoost );
				float BackTexColorRangeOverlay516 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _BackTexColorRangeOverlayFrequency ) + ( _BackTexColorRangeOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _BackTexColorRangeOverlayStrength );
				float2 temp_cast_1 = (_BackTexScale).xx;
				float mulTime379 = _TimeParameters.x * (float)_DynamicBackTecRotationSpeed;
				float2 appendResult377 = (float2(( ( mulTime379 * _BackTexRotationSpeed ) + _BackTexRotationSpeed ) , ( _BackTexMoveToCenterSpeed + ( _BackTexMoveToCenterSpeed * mulTime379 ) )));
				float2 texCoord378 = IN.ase_texcoord2.zw * temp_cast_1 + ( ( ( _BackTexScale * -0.5 ) + 0.5 ) + appendResult377 );
				float3 rotatedValue580 = RotateAroundAxis( float3( 0.5,0.5,0.5 ), float3( texCoord378 ,  0.0 ), normalize( float3( 0,0,1 ) ), ( ( _BackTexFixedRotation * PI ) * 0.5 ) );
				float4 tex2DNode38 = tex2D( _BackTexture, rotatedValue580.xy );
				float BackTextureColorRange369 = saturate( (0.0 + (( ( tex2DNode38.r + tex2DNode38.g + tex2DNode38.b ) / 3.0 ) - _BackTexValueRemap.x) * (1.0 - 0.0) / (_BackTexValueRemap.y - _BackTexValueRemap.x)) );
				float ColorRangeFinal543 = saturate( ( saturate( ( temp_output_533_0 + ( temp_output_533_0 * RadialSwingColorRangeBoost573 ) ) ) + saturate( ( BackTexColorRangeOverlay516 * BackTextureColorRange369 ) ) ) );
				float4 lerpResult547 = lerp( _Color_1 , _Color_2 , saturate( (0.0 + (ColorRangeFinal543 - _Color_2_Start) * (1.0 - 0.0) / (_Color_3_Start - _Color_2_Start)) ));
				float4 lerpResult550 = lerp( _Color_2 , _Color3 , saturate( (0.0 + (ColorRangeFinal543 - _Color_3_Start) * (1.0 - 0.0) / (_Color_3_Full - _Color_3_Start)) ));
				float4 temp_output_404_0 = ( ( ColorRangeFinal543 < _Color_2_Start ? _Color_1 : ( ColorRangeFinal543 < _Color_3_Start ? lerpResult547 : ( ColorRangeFinal543 < _Color_3_Full ? lerpResult550 : _Color3 ) ) ) * _ShowMaster );
				float dotResult590 = dot( temp_output_404_0 , float4( 1,1,1,0 ) );
				

				float Alpha = ( dotResult590 >= _Float0 ? 1.0 : 0.0 );
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
					LODFadeCrossFade( IN.clipPos );
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
			#define _SURFACE_TYPE_TRANSPARENT 1
			#define ASE_SRP_VERSION 140009


			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"

			#define ASE_NEEDS_FRAG_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
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
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color_2;
			float4 _Color_1;
			float2 _BackTexValueRemap;
			float2 _RingTexFixedOffset;
			float2 _RingTexValueRemap;
			float _RingTexScale;
			float _Color_3_Full;
			float _Color_3_Start;
			float _Color_2_Start;
			float _BackTexMoveToCenterSpeed;
			float _BackTexRotationSpeed;
			int _DynamicBackTecRotationSpeed;
			float _BackTexScale;
			float _BackTexFixedRotation;
			float _BackTexColorRangeOverlayOffset;
			float _ShowMaster;
			float _BackTexColorRangeOverlayFrequency;
			float _RingSwingColorRangeBoost;
			float _RingTexRotation;
			float _RingSwingExtraWobble;
			float _RingSwingSpeed;
			float _RingSwingStrength;
			float _RingWobbleStrength;
			float _RingWobbleSpeed;
			float _RingWobbleOverlayStrength;
			float _RingWobbleOverlayOffset;
			float _RingWobbleOverlayFrequency;
			float _BackTexColorRangeOverlayStrength;
			float _Float0;
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
			sampler2D _RingWobbleTex;
			sampler2D _RingSwingTex;
			sampler2D _BackTexture;


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

				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_texcoord3 = v.vertex;
				o.ase_texcoord2.zw = v.ase_texcoord1.xy;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

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
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;

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
				o.ase_texcoord = v.ase_texcoord;
				o.ase_texcoord1 = v.ase_texcoord1;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
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

				float2 temp_cast_0 = (_RingTexScale).xx;
				float2 appendResult342 = (float2(IN.ase_texcoord3.xyz.x , IN.ase_texcoord3.xyz.z));
				float Vertex_Distance_From_Center_Relative345 = ( length( appendResult342 ) / 0.7 );
				float WobbleRadiusMulti435 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _RingWobbleOverlayFrequency ) + ( _RingWobbleOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _RingWobbleOverlayStrength );
				float2 appendResult56 = (float2(IN.ase_texcoord3.xyz.x , IN.ase_texcoord3.xyz.z));
				float2 normalizeResult120 = normalize( appendResult56 );
				float2 break221 = normalizeResult120;
				float2 _SeamVector = float2(0,1);
				float dotResult117 = dot( normalizeResult120 , _SeamVector );
				float angle204 = (0.0 + (atan2( ( ( break221.x * 1.0 * _SeamVector.y ) - ( break221.y * _SeamVector.x ) ) , dotResult117 ) - ( -1.0 * PI )) * (1.0 - 0.0) / (PI - ( -1.0 * PI )));
				float2 appendResult425 = (float2(angle204 , ( _RingWobbleSpeed + ( _RingWobbleSpeed * _TimeParameters.x ) )));
				float4 tex2DNode424 = tex2D( _RingWobbleTex, appendResult425 );
				float2 appendResult429 = (float2(IN.ase_texcoord3.xyz.x , IN.ase_texcoord3.xyz.z));
				float2 normalizeResult430 = normalize( appendResult429 );
				float2 appendResult453 = (float2(( angle204 + ( _RingSwingSpeed + ( _RingSwingSpeed * _TimeParameters.x ) ) ) , ( _RingSwingExtraWobble * _TimeParameters.x )));
				float4 tex2DNode452 = tex2D( _RingSwingTex, appendResult453 );
				float temp_output_459_0 = ( _RingSwingStrength * ( ( tex2DNode452.r + tex2DNode452.g + tex2DNode452.b ) / 3.0 ) );
				float temp_output_589_0 = ( WobbleRadiusMulti435 * temp_output_459_0 );
				float2 Radial_Wobble_Offset432 = ( ( ( WobbleRadiusMulti435 * ( ( ( tex2DNode424.r + tex2DNode424.g + tex2DNode424.b ) / 3.0 ) * _RingWobbleStrength ) ) * normalizeResult430 ) + ( normalizeResult430 * temp_output_589_0 ) );
				float2 texCoord319 = IN.ase_texcoord2.xy * temp_cast_0 + ( ( ( _RingTexScale * -0.5 ) + 0.5 ) + Radial_Wobble_Offset432 + _RingTexFixedOffset );
				float cos320 = cos( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float sin320 = sin( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float2 rotator320 = mul( texCoord319 - float2( 0.5,0.5 ) , float2x2( cos320 , -sin320 , sin320 , cos320 )) + float2( 0.5,0.5 );
				float4 break483 = tex2D( _RingTexture, rotator320 );
				float temp_output_533_0 = (0.0 + (( ( break483.r + break483.g + break483.b ) / 3.0 ) - _RingTexValueRemap.x) * (1.0 - 0.0) / (_RingTexValueRemap.y - _RingTexValueRemap.x));
				float RadialSwingColorRangeBoost573 = ( ( abs( temp_output_589_0 ) + abs( temp_output_459_0 ) ) * _RingSwingColorRangeBoost );
				float BackTexColorRangeOverlay516 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _BackTexColorRangeOverlayFrequency ) + ( _BackTexColorRangeOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _BackTexColorRangeOverlayStrength );
				float2 temp_cast_1 = (_BackTexScale).xx;
				float mulTime379 = _TimeParameters.x * (float)_DynamicBackTecRotationSpeed;
				float2 appendResult377 = (float2(( ( mulTime379 * _BackTexRotationSpeed ) + _BackTexRotationSpeed ) , ( _BackTexMoveToCenterSpeed + ( _BackTexMoveToCenterSpeed * mulTime379 ) )));
				float2 texCoord378 = IN.ase_texcoord2.zw * temp_cast_1 + ( ( ( _BackTexScale * -0.5 ) + 0.5 ) + appendResult377 );
				float3 rotatedValue580 = RotateAroundAxis( float3( 0.5,0.5,0.5 ), float3( texCoord378 ,  0.0 ), normalize( float3( 0,0,1 ) ), ( ( _BackTexFixedRotation * PI ) * 0.5 ) );
				float4 tex2DNode38 = tex2D( _BackTexture, rotatedValue580.xy );
				float BackTextureColorRange369 = saturate( (0.0 + (( ( tex2DNode38.r + tex2DNode38.g + tex2DNode38.b ) / 3.0 ) - _BackTexValueRemap.x) * (1.0 - 0.0) / (_BackTexValueRemap.y - _BackTexValueRemap.x)) );
				float ColorRangeFinal543 = saturate( ( saturate( ( temp_output_533_0 + ( temp_output_533_0 * RadialSwingColorRangeBoost573 ) ) ) + saturate( ( BackTexColorRangeOverlay516 * BackTextureColorRange369 ) ) ) );
				float4 lerpResult547 = lerp( _Color_1 , _Color_2 , saturate( (0.0 + (ColorRangeFinal543 - _Color_2_Start) * (1.0 - 0.0) / (_Color_3_Start - _Color_2_Start)) ));
				float4 lerpResult550 = lerp( _Color_2 , _Color3 , saturate( (0.0 + (ColorRangeFinal543 - _Color_3_Start) * (1.0 - 0.0) / (_Color_3_Full - _Color_3_Start)) ));
				float4 temp_output_404_0 = ( ( ColorRangeFinal543 < _Color_2_Start ? _Color_1 : ( ColorRangeFinal543 < _Color_3_Start ? lerpResult547 : ( ColorRangeFinal543 < _Color_3_Full ? lerpResult550 : _Color3 ) ) ) * _ShowMaster );
				float dotResult590 = dot( temp_output_404_0 , float4( 1,1,1,0 ) );
				

				float Alpha = ( dotResult590 >= _Float0 ? 1.0 : 0.0 );
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

			#define ASE_FOG 1
			#define _SURFACE_TYPE_TRANSPARENT 1
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

			#define ASE_NEEDS_FRAG_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color_2;
			float4 _Color_1;
			float2 _BackTexValueRemap;
			float2 _RingTexFixedOffset;
			float2 _RingTexValueRemap;
			float _RingTexScale;
			float _Color_3_Full;
			float _Color_3_Start;
			float _Color_2_Start;
			float _BackTexMoveToCenterSpeed;
			float _BackTexRotationSpeed;
			int _DynamicBackTecRotationSpeed;
			float _BackTexScale;
			float _BackTexFixedRotation;
			float _BackTexColorRangeOverlayOffset;
			float _ShowMaster;
			float _BackTexColorRangeOverlayFrequency;
			float _RingSwingColorRangeBoost;
			float _RingTexRotation;
			float _RingSwingExtraWobble;
			float _RingSwingSpeed;
			float _RingSwingStrength;
			float _RingWobbleStrength;
			float _RingWobbleSpeed;
			float _RingWobbleOverlayStrength;
			float _RingWobbleOverlayOffset;
			float _RingWobbleOverlayFrequency;
			float _BackTexColorRangeOverlayStrength;
			float _Float0;
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
			sampler2D _RingWobbleTex;
			sampler2D _RingSwingTex;
			sampler2D _BackTexture;


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

				o.ase_texcoord.xy = v.ase_texcoord.xy;
				o.ase_texcoord1 = v.vertex;
				o.ase_texcoord.zw = v.ase_texcoord1.xy;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

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
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;

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
				o.ase_texcoord = v.ase_texcoord;
				o.ase_texcoord1 = v.ase_texcoord1;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
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

				float2 temp_cast_0 = (_RingTexScale).xx;
				float2 appendResult342 = (float2(IN.ase_texcoord1.xyz.x , IN.ase_texcoord1.xyz.z));
				float Vertex_Distance_From_Center_Relative345 = ( length( appendResult342 ) / 0.7 );
				float WobbleRadiusMulti435 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _RingWobbleOverlayFrequency ) + ( _RingWobbleOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _RingWobbleOverlayStrength );
				float2 appendResult56 = (float2(IN.ase_texcoord1.xyz.x , IN.ase_texcoord1.xyz.z));
				float2 normalizeResult120 = normalize( appendResult56 );
				float2 break221 = normalizeResult120;
				float2 _SeamVector = float2(0,1);
				float dotResult117 = dot( normalizeResult120 , _SeamVector );
				float angle204 = (0.0 + (atan2( ( ( break221.x * 1.0 * _SeamVector.y ) - ( break221.y * _SeamVector.x ) ) , dotResult117 ) - ( -1.0 * PI )) * (1.0 - 0.0) / (PI - ( -1.0 * PI )));
				float2 appendResult425 = (float2(angle204 , ( _RingWobbleSpeed + ( _RingWobbleSpeed * _TimeParameters.x ) )));
				float4 tex2DNode424 = tex2D( _RingWobbleTex, appendResult425 );
				float2 appendResult429 = (float2(IN.ase_texcoord1.xyz.x , IN.ase_texcoord1.xyz.z));
				float2 normalizeResult430 = normalize( appendResult429 );
				float2 appendResult453 = (float2(( angle204 + ( _RingSwingSpeed + ( _RingSwingSpeed * _TimeParameters.x ) ) ) , ( _RingSwingExtraWobble * _TimeParameters.x )));
				float4 tex2DNode452 = tex2D( _RingSwingTex, appendResult453 );
				float temp_output_459_0 = ( _RingSwingStrength * ( ( tex2DNode452.r + tex2DNode452.g + tex2DNode452.b ) / 3.0 ) );
				float temp_output_589_0 = ( WobbleRadiusMulti435 * temp_output_459_0 );
				float2 Radial_Wobble_Offset432 = ( ( ( WobbleRadiusMulti435 * ( ( ( tex2DNode424.r + tex2DNode424.g + tex2DNode424.b ) / 3.0 ) * _RingWobbleStrength ) ) * normalizeResult430 ) + ( normalizeResult430 * temp_output_589_0 ) );
				float2 texCoord319 = IN.ase_texcoord.xy * temp_cast_0 + ( ( ( _RingTexScale * -0.5 ) + 0.5 ) + Radial_Wobble_Offset432 + _RingTexFixedOffset );
				float cos320 = cos( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float sin320 = sin( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float2 rotator320 = mul( texCoord319 - float2( 0.5,0.5 ) , float2x2( cos320 , -sin320 , sin320 , cos320 )) + float2( 0.5,0.5 );
				float4 break483 = tex2D( _RingTexture, rotator320 );
				float temp_output_533_0 = (0.0 + (( ( break483.r + break483.g + break483.b ) / 3.0 ) - _RingTexValueRemap.x) * (1.0 - 0.0) / (_RingTexValueRemap.y - _RingTexValueRemap.x));
				float RadialSwingColorRangeBoost573 = ( ( abs( temp_output_589_0 ) + abs( temp_output_459_0 ) ) * _RingSwingColorRangeBoost );
				float BackTexColorRangeOverlay516 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _BackTexColorRangeOverlayFrequency ) + ( _BackTexColorRangeOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _BackTexColorRangeOverlayStrength );
				float2 temp_cast_1 = (_BackTexScale).xx;
				float mulTime379 = _TimeParameters.x * (float)_DynamicBackTecRotationSpeed;
				float2 appendResult377 = (float2(( ( mulTime379 * _BackTexRotationSpeed ) + _BackTexRotationSpeed ) , ( _BackTexMoveToCenterSpeed + ( _BackTexMoveToCenterSpeed * mulTime379 ) )));
				float2 texCoord378 = IN.ase_texcoord.zw * temp_cast_1 + ( ( ( _BackTexScale * -0.5 ) + 0.5 ) + appendResult377 );
				float3 rotatedValue580 = RotateAroundAxis( float3( 0.5,0.5,0.5 ), float3( texCoord378 ,  0.0 ), normalize( float3( 0,0,1 ) ), ( ( _BackTexFixedRotation * PI ) * 0.5 ) );
				float4 tex2DNode38 = tex2D( _BackTexture, rotatedValue580.xy );
				float BackTextureColorRange369 = saturate( (0.0 + (( ( tex2DNode38.r + tex2DNode38.g + tex2DNode38.b ) / 3.0 ) - _BackTexValueRemap.x) * (1.0 - 0.0) / (_BackTexValueRemap.y - _BackTexValueRemap.x)) );
				float ColorRangeFinal543 = saturate( ( saturate( ( temp_output_533_0 + ( temp_output_533_0 * RadialSwingColorRangeBoost573 ) ) ) + saturate( ( BackTexColorRangeOverlay516 * BackTextureColorRange369 ) ) ) );
				float4 lerpResult547 = lerp( _Color_1 , _Color_2 , saturate( (0.0 + (ColorRangeFinal543 - _Color_2_Start) * (1.0 - 0.0) / (_Color_3_Start - _Color_2_Start)) ));
				float4 lerpResult550 = lerp( _Color_2 , _Color3 , saturate( (0.0 + (ColorRangeFinal543 - _Color_3_Start) * (1.0 - 0.0) / (_Color_3_Full - _Color_3_Start)) ));
				float4 temp_output_404_0 = ( ( ColorRangeFinal543 < _Color_2_Start ? _Color_1 : ( ColorRangeFinal543 < _Color_3_Start ? lerpResult547 : ( ColorRangeFinal543 < _Color_3_Full ? lerpResult550 : _Color3 ) ) ) * _ShowMaster );
				float dotResult590 = dot( temp_output_404_0 , float4( 1,1,1,0 ) );
				

				surfaceDescription.Alpha = ( dotResult590 >= _Float0 ? 1.0 : 0.0 );
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
			#define _SURFACE_TYPE_TRANSPARENT 1
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

			#define ASE_NEEDS_FRAG_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color_2;
			float4 _Color_1;
			float2 _BackTexValueRemap;
			float2 _RingTexFixedOffset;
			float2 _RingTexValueRemap;
			float _RingTexScale;
			float _Color_3_Full;
			float _Color_3_Start;
			float _Color_2_Start;
			float _BackTexMoveToCenterSpeed;
			float _BackTexRotationSpeed;
			int _DynamicBackTecRotationSpeed;
			float _BackTexScale;
			float _BackTexFixedRotation;
			float _BackTexColorRangeOverlayOffset;
			float _ShowMaster;
			float _BackTexColorRangeOverlayFrequency;
			float _RingSwingColorRangeBoost;
			float _RingTexRotation;
			float _RingSwingExtraWobble;
			float _RingSwingSpeed;
			float _RingSwingStrength;
			float _RingWobbleStrength;
			float _RingWobbleSpeed;
			float _RingWobbleOverlayStrength;
			float _RingWobbleOverlayOffset;
			float _RingWobbleOverlayFrequency;
			float _BackTexColorRangeOverlayStrength;
			float _Float0;
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
			sampler2D _RingWobbleTex;
			sampler2D _RingSwingTex;
			sampler2D _BackTexture;


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

				o.ase_texcoord.xy = v.ase_texcoord.xy;
				o.ase_texcoord1 = v.vertex;
				o.ase_texcoord.zw = v.ase_texcoord1.xy;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

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
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;

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
				o.ase_texcoord = v.ase_texcoord;
				o.ase_texcoord1 = v.ase_texcoord1;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
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

				float2 temp_cast_0 = (_RingTexScale).xx;
				float2 appendResult342 = (float2(IN.ase_texcoord1.xyz.x , IN.ase_texcoord1.xyz.z));
				float Vertex_Distance_From_Center_Relative345 = ( length( appendResult342 ) / 0.7 );
				float WobbleRadiusMulti435 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _RingWobbleOverlayFrequency ) + ( _RingWobbleOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _RingWobbleOverlayStrength );
				float2 appendResult56 = (float2(IN.ase_texcoord1.xyz.x , IN.ase_texcoord1.xyz.z));
				float2 normalizeResult120 = normalize( appendResult56 );
				float2 break221 = normalizeResult120;
				float2 _SeamVector = float2(0,1);
				float dotResult117 = dot( normalizeResult120 , _SeamVector );
				float angle204 = (0.0 + (atan2( ( ( break221.x * 1.0 * _SeamVector.y ) - ( break221.y * _SeamVector.x ) ) , dotResult117 ) - ( -1.0 * PI )) * (1.0 - 0.0) / (PI - ( -1.0 * PI )));
				float2 appendResult425 = (float2(angle204 , ( _RingWobbleSpeed + ( _RingWobbleSpeed * _TimeParameters.x ) )));
				float4 tex2DNode424 = tex2D( _RingWobbleTex, appendResult425 );
				float2 appendResult429 = (float2(IN.ase_texcoord1.xyz.x , IN.ase_texcoord1.xyz.z));
				float2 normalizeResult430 = normalize( appendResult429 );
				float2 appendResult453 = (float2(( angle204 + ( _RingSwingSpeed + ( _RingSwingSpeed * _TimeParameters.x ) ) ) , ( _RingSwingExtraWobble * _TimeParameters.x )));
				float4 tex2DNode452 = tex2D( _RingSwingTex, appendResult453 );
				float temp_output_459_0 = ( _RingSwingStrength * ( ( tex2DNode452.r + tex2DNode452.g + tex2DNode452.b ) / 3.0 ) );
				float temp_output_589_0 = ( WobbleRadiusMulti435 * temp_output_459_0 );
				float2 Radial_Wobble_Offset432 = ( ( ( WobbleRadiusMulti435 * ( ( ( tex2DNode424.r + tex2DNode424.g + tex2DNode424.b ) / 3.0 ) * _RingWobbleStrength ) ) * normalizeResult430 ) + ( normalizeResult430 * temp_output_589_0 ) );
				float2 texCoord319 = IN.ase_texcoord.xy * temp_cast_0 + ( ( ( _RingTexScale * -0.5 ) + 0.5 ) + Radial_Wobble_Offset432 + _RingTexFixedOffset );
				float cos320 = cos( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float sin320 = sin( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float2 rotator320 = mul( texCoord319 - float2( 0.5,0.5 ) , float2x2( cos320 , -sin320 , sin320 , cos320 )) + float2( 0.5,0.5 );
				float4 break483 = tex2D( _RingTexture, rotator320 );
				float temp_output_533_0 = (0.0 + (( ( break483.r + break483.g + break483.b ) / 3.0 ) - _RingTexValueRemap.x) * (1.0 - 0.0) / (_RingTexValueRemap.y - _RingTexValueRemap.x));
				float RadialSwingColorRangeBoost573 = ( ( abs( temp_output_589_0 ) + abs( temp_output_459_0 ) ) * _RingSwingColorRangeBoost );
				float BackTexColorRangeOverlay516 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _BackTexColorRangeOverlayFrequency ) + ( _BackTexColorRangeOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _BackTexColorRangeOverlayStrength );
				float2 temp_cast_1 = (_BackTexScale).xx;
				float mulTime379 = _TimeParameters.x * (float)_DynamicBackTecRotationSpeed;
				float2 appendResult377 = (float2(( ( mulTime379 * _BackTexRotationSpeed ) + _BackTexRotationSpeed ) , ( _BackTexMoveToCenterSpeed + ( _BackTexMoveToCenterSpeed * mulTime379 ) )));
				float2 texCoord378 = IN.ase_texcoord.zw * temp_cast_1 + ( ( ( _BackTexScale * -0.5 ) + 0.5 ) + appendResult377 );
				float3 rotatedValue580 = RotateAroundAxis( float3( 0.5,0.5,0.5 ), float3( texCoord378 ,  0.0 ), normalize( float3( 0,0,1 ) ), ( ( _BackTexFixedRotation * PI ) * 0.5 ) );
				float4 tex2DNode38 = tex2D( _BackTexture, rotatedValue580.xy );
				float BackTextureColorRange369 = saturate( (0.0 + (( ( tex2DNode38.r + tex2DNode38.g + tex2DNode38.b ) / 3.0 ) - _BackTexValueRemap.x) * (1.0 - 0.0) / (_BackTexValueRemap.y - _BackTexValueRemap.x)) );
				float ColorRangeFinal543 = saturate( ( saturate( ( temp_output_533_0 + ( temp_output_533_0 * RadialSwingColorRangeBoost573 ) ) ) + saturate( ( BackTexColorRangeOverlay516 * BackTextureColorRange369 ) ) ) );
				float4 lerpResult547 = lerp( _Color_1 , _Color_2 , saturate( (0.0 + (ColorRangeFinal543 - _Color_2_Start) * (1.0 - 0.0) / (_Color_3_Start - _Color_2_Start)) ));
				float4 lerpResult550 = lerp( _Color_2 , _Color3 , saturate( (0.0 + (ColorRangeFinal543 - _Color_3_Start) * (1.0 - 0.0) / (_Color_3_Full - _Color_3_Start)) ));
				float4 temp_output_404_0 = ( ( ColorRangeFinal543 < _Color_2_Start ? _Color_1 : ( ColorRangeFinal543 < _Color_3_Start ? lerpResult547 : ( ColorRangeFinal543 < _Color_3_Full ? lerpResult550 : _Color3 ) ) ) * _ShowMaster );
				float dotResult590 = dot( temp_output_404_0 , float4( 1,1,1,0 ) );
				

				surfaceDescription.Alpha = ( dotResult590 >= _Float0 ? 1.0 : 0.0 );
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
			#define _SURFACE_TYPE_TRANSPARENT 1
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

			#define ASE_NEEDS_FRAG_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float3 normalWS : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color3;
			float4 _Color_2;
			float4 _Color_1;
			float2 _BackTexValueRemap;
			float2 _RingTexFixedOffset;
			float2 _RingTexValueRemap;
			float _RingTexScale;
			float _Color_3_Full;
			float _Color_3_Start;
			float _Color_2_Start;
			float _BackTexMoveToCenterSpeed;
			float _BackTexRotationSpeed;
			int _DynamicBackTecRotationSpeed;
			float _BackTexScale;
			float _BackTexFixedRotation;
			float _BackTexColorRangeOverlayOffset;
			float _ShowMaster;
			float _BackTexColorRangeOverlayFrequency;
			float _RingSwingColorRangeBoost;
			float _RingTexRotation;
			float _RingSwingExtraWobble;
			float _RingSwingSpeed;
			float _RingSwingStrength;
			float _RingWobbleStrength;
			float _RingWobbleSpeed;
			float _RingWobbleOverlayStrength;
			float _RingWobbleOverlayOffset;
			float _RingWobbleOverlayFrequency;
			float _BackTexColorRangeOverlayStrength;
			float _Float0;
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
			sampler2D _RingWobbleTex;
			sampler2D _RingSwingTex;
			sampler2D _BackTexture;


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

				o.ase_texcoord1.xy = v.ase_texcoord.xy;
				o.ase_texcoord2 = v.vertex;
				o.ase_texcoord1.zw = v.ase_texcoord1.xy;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = defaultVertexValue;

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
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;

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
				o.ase_texcoord = v.ase_texcoord;
				o.ase_texcoord1 = v.ase_texcoord1;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
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

				float2 temp_cast_0 = (_RingTexScale).xx;
				float2 appendResult342 = (float2(IN.ase_texcoord2.xyz.x , IN.ase_texcoord2.xyz.z));
				float Vertex_Distance_From_Center_Relative345 = ( length( appendResult342 ) / 0.7 );
				float WobbleRadiusMulti435 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _RingWobbleOverlayFrequency ) + ( _RingWobbleOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _RingWobbleOverlayStrength );
				float2 appendResult56 = (float2(IN.ase_texcoord2.xyz.x , IN.ase_texcoord2.xyz.z));
				float2 normalizeResult120 = normalize( appendResult56 );
				float2 break221 = normalizeResult120;
				float2 _SeamVector = float2(0,1);
				float dotResult117 = dot( normalizeResult120 , _SeamVector );
				float angle204 = (0.0 + (atan2( ( ( break221.x * 1.0 * _SeamVector.y ) - ( break221.y * _SeamVector.x ) ) , dotResult117 ) - ( -1.0 * PI )) * (1.0 - 0.0) / (PI - ( -1.0 * PI )));
				float2 appendResult425 = (float2(angle204 , ( _RingWobbleSpeed + ( _RingWobbleSpeed * _TimeParameters.x ) )));
				float4 tex2DNode424 = tex2D( _RingWobbleTex, appendResult425 );
				float2 appendResult429 = (float2(IN.ase_texcoord2.xyz.x , IN.ase_texcoord2.xyz.z));
				float2 normalizeResult430 = normalize( appendResult429 );
				float2 appendResult453 = (float2(( angle204 + ( _RingSwingSpeed + ( _RingSwingSpeed * _TimeParameters.x ) ) ) , ( _RingSwingExtraWobble * _TimeParameters.x )));
				float4 tex2DNode452 = tex2D( _RingSwingTex, appendResult453 );
				float temp_output_459_0 = ( _RingSwingStrength * ( ( tex2DNode452.r + tex2DNode452.g + tex2DNode452.b ) / 3.0 ) );
				float temp_output_589_0 = ( WobbleRadiusMulti435 * temp_output_459_0 );
				float2 Radial_Wobble_Offset432 = ( ( ( WobbleRadiusMulti435 * ( ( ( tex2DNode424.r + tex2DNode424.g + tex2DNode424.b ) / 3.0 ) * _RingWobbleStrength ) ) * normalizeResult430 ) + ( normalizeResult430 * temp_output_589_0 ) );
				float2 texCoord319 = IN.ase_texcoord1.xy * temp_cast_0 + ( ( ( _RingTexScale * -0.5 ) + 0.5 ) + Radial_Wobble_Offset432 + _RingTexFixedOffset );
				float cos320 = cos( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float sin320 = sin( ( ( _RingTexRotation * ( 2.0 * PI ) * _TimeParameters.x ) + 0.0 ) );
				float2 rotator320 = mul( texCoord319 - float2( 0.5,0.5 ) , float2x2( cos320 , -sin320 , sin320 , cos320 )) + float2( 0.5,0.5 );
				float4 break483 = tex2D( _RingTexture, rotator320 );
				float temp_output_533_0 = (0.0 + (( ( break483.r + break483.g + break483.b ) / 3.0 ) - _RingTexValueRemap.x) * (1.0 - 0.0) / (_RingTexValueRemap.y - _RingTexValueRemap.x));
				float RadialSwingColorRangeBoost573 = ( ( abs( temp_output_589_0 ) + abs( temp_output_459_0 ) ) * _RingSwingColorRangeBoost );
				float BackTexColorRangeOverlay516 = ( ( sin( ( ( ( Vertex_Distance_From_Center_Relative345 * _BackTexColorRangeOverlayFrequency ) + ( _BackTexColorRangeOverlayOffset * 2.0 ) ) * PI ) ) + 1.0 ) * 0.5 * _BackTexColorRangeOverlayStrength );
				float2 temp_cast_1 = (_BackTexScale).xx;
				float mulTime379 = _TimeParameters.x * (float)_DynamicBackTecRotationSpeed;
				float2 appendResult377 = (float2(( ( mulTime379 * _BackTexRotationSpeed ) + _BackTexRotationSpeed ) , ( _BackTexMoveToCenterSpeed + ( _BackTexMoveToCenterSpeed * mulTime379 ) )));
				float2 texCoord378 = IN.ase_texcoord1.zw * temp_cast_1 + ( ( ( _BackTexScale * -0.5 ) + 0.5 ) + appendResult377 );
				float3 rotatedValue580 = RotateAroundAxis( float3( 0.5,0.5,0.5 ), float3( texCoord378 ,  0.0 ), normalize( float3( 0,0,1 ) ), ( ( _BackTexFixedRotation * PI ) * 0.5 ) );
				float4 tex2DNode38 = tex2D( _BackTexture, rotatedValue580.xy );
				float BackTextureColorRange369 = saturate( (0.0 + (( ( tex2DNode38.r + tex2DNode38.g + tex2DNode38.b ) / 3.0 ) - _BackTexValueRemap.x) * (1.0 - 0.0) / (_BackTexValueRemap.y - _BackTexValueRemap.x)) );
				float ColorRangeFinal543 = saturate( ( saturate( ( temp_output_533_0 + ( temp_output_533_0 * RadialSwingColorRangeBoost573 ) ) ) + saturate( ( BackTexColorRangeOverlay516 * BackTextureColorRange369 ) ) ) );
				float4 lerpResult547 = lerp( _Color_1 , _Color_2 , saturate( (0.0 + (ColorRangeFinal543 - _Color_2_Start) * (1.0 - 0.0) / (_Color_3_Start - _Color_2_Start)) ));
				float4 lerpResult550 = lerp( _Color_2 , _Color3 , saturate( (0.0 + (ColorRangeFinal543 - _Color_3_Start) * (1.0 - 0.0) / (_Color_3_Full - _Color_3_Start)) ));
				float4 temp_output_404_0 = ( ( ColorRangeFinal543 < _Color_2_Start ? _Color_1 : ( ColorRangeFinal543 < _Color_3_Start ? lerpResult547 : ( ColorRangeFinal543 < _Color_3_Full ? lerpResult550 : _Color3 ) ) ) * _ShowMaster );
				float dotResult590 = dot( temp_output_404_0 , float4( 1,1,1,0 ) );
				

				surfaceDescription.Alpha = ( dotResult590 >= _Float0 ? 1.0 : 0.0 );
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
Node;AmplifyShaderEditor.CommentaryNode;553;3402.645,-4506.527;Inherit;False;1902.793;1305.054;Comment;18;544;539;474;493;545;546;549;548;478;547;487;481;550;551;552;486;477;542;Assign Colors to Color Range;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;498;-970.9894,-2901.518;Inherit;False;1745.463;514.3179;Comment;12;445;441;447;438;446;443;435;448;444;436;437;497;RadialOffsetOverlay;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;422;-977.5175,-2282.62;Inherit;False;1776.722;868.4512;Comment;26;236;268;423;424;425;427;428;429;430;434;449;450;426;452;453;455;456;457;459;463;433;568;567;569;570;589;Radial Wobble;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;389;1174.108,-3096.371;Inherit;False;1307.705;412.9607;Comment;8;360;363;362;361;348;319;494;496;TextureScale;0.2745416,0.350211,0.4245283,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;349;-3090.427,-2841.338;Inherit;False;1363.052;329.877;Comment;6;341;342;344;343;345;340;Distance From Center Relative;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;338;1243.362,-2475.183;Inherit;False;2486.851;784.7704;Comment;14;485;484;483;318;320;394;322;323;324;325;533;472;575;576;Ring Texture;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;313;-3369.373,-2323.563;Inherit;False;2078.093;937.7032;Get any angle on the circle;14;204;252;254;253;225;421;117;120;226;222;223;221;56;203;Angle;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;175;255.9677,-956.07;Inherit;False;1914.884;836.5123;Comment;19;49;50;38;379;377;378;376;375;417;418;501;502;503;504;509;580;581;582;583;BackTexture;0.167702,0.4790962,0.6226415,1;0;0
Node;AmplifyShaderEditor.PiNode;325;1467.395,-2028.206;Inherit;False;1;0;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;324;1835.299,-2168.166;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;376;618.5233,-835.0846;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;15.89;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;375;614.9955,-734.705;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;-1.2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;417;811.5376,-875.5546;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;418;803.8907,-700.5285;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;379;411.3051,-776.3256;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;323;1475.781,-2186.292;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;85;405.8675,373.4758;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;86;405.8675,373.4758;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;87;405.8675,373.4758;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;88;405.8675,373.4758;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;89;405.8675,373.4758;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;90;405.8675,373.4758;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;91;405.8675,373.4758;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;92;405.8675,373.4758;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.SimpleAddOpNode;363;1992.286,-3058.246;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;361;1529.894,-3067.845;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;-0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;496;1757.406,-2778.531;Inherit;False;Property;_RingTexFixedOffset;RingTexFixedOffset;11;0;Create;True;0;0;0;False;0;False;0,0;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleAddOpNode;362;1733.827,-3063.328;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;494;2011.501,-2954.673;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT2;0.5,0;False;2;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;509;1997.535,-746.2872;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;508;2164.116,-714.6019;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;512;2630.933,-666.8;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;511;2418.584,-693.304;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;369;2568.917,-1000.609;Inherit;False;BackTextureColorRange;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;83;1328.477,1909.457;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.PosVertexDataNode;341;-3040.426,-2791.338;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;342;-2795.123,-2757.948;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;344;-2336.78,-2731.171;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LengthOpNode;343;-2550.768,-2733.238;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;340;-2560.995,-2624.261;Inherit;False;Constant;_RadiusAbsolute;Radius Absolute;37;0;Create;True;0;0;0;False;0;False;0.7;0;0.7;0.7;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;463;-541.1423,-1583.758;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalizeNode;430;35.22259,-1857.239;Inherit;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;423;-608.5024,-2215.792;Inherit;False;204;angle;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;455;-602.2534,-2007.986;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;345;-2096.123,-2786.275;Inherit;False;Vertex Distance From Center Relative;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;460;-703.1621,-1434.31;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;461;-899.1671,-1306.057;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;456;-952.5039,-1817.438;Inherit;False;204;angle;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;437;353.6161,-2672.988;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0.5;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;445;166.6884,-2752.966;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;441;-192.9358,-2648.737;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;447;-556.3117,-2697.966;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;438;-382.1778,-2606.353;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;446;-560.8852,-2567.961;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;443;-910.7293,-2851.519;Inherit;False;345;Vertex Distance From Center Relative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;436;-29.21198,-2731.691;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;515;3212.467,-1272.904;Inherit;False;1745.463;514.3179;Comment;12;527;526;525;524;523;522;521;520;519;518;517;516;BackTextureColorRangeOverlay;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;499;5546.483,-2728.759;Inherit;False;369;BackTextureColorRange;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;404;5121.624,-2402.526;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SaturateNode;538;4325.48,-2537.426;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;543;4476.835,-2694.457;Inherit;False;ColorRangeFinal;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;518;4350.145,-1124.352;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;520;3627.144,-1069.352;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;521;3801.278,-977.7393;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;522;3622.571,-939.3472;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;524;4154.244,-1103.077;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;523;3271.727,-1222.905;Inherit;False;345;Vertex Distance From Center Relative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;526;3264.395,-1052.082;Inherit;False;Property;_BackTexColorRangeOverlayOffset;BackTexColorRangeOverlayOffset;34;0;Create;True;0;0;0;False;0;False;0;0.084;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;517;4477.511,-1055.671;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0.5;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;516;4600.178,-889.9699;Inherit;False;BackTexColorRangeOverlay;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;527;3262.467,-1142.736;Inherit;False;Property;_BackTexColorRangeOverlayFrequency;BackTexColorRangeOverlayFrequency;35;0;Create;True;0;0;0;False;0;False;0;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;519;3961.854,-1031.271;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;525;4141.782,-943.0279;Inherit;False;Property;_BackTexColorRangeOverlayStrength;BackTexColorRangeOverlayStrength;33;0;Create;True;0;0;0;False;0;False;0;4.07;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;554;3203.572,-678.9767;Inherit;False;1745.463;514.3179;Comment;12;566;565;564;563;562;561;560;559;558;557;556;555;BackTextureColorRangeOverlay;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;555;4341.25,-530.4247;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;556;3618.249,-475.4247;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;557;3792.383,-383.8119;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;558;3613.676,-345.4198;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;559;4145.348,-509.1497;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;560;3262.832,-628.9777;Inherit;False;345;Vertex Distance From Center Relative;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;562;4468.616,-461.7436;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0.5;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;565;3952.959,-437.3436;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;564;3253.572,-548.8085;Inherit;False;Property;_RingTexColorRangeOverlayFrequency;RingTexColorRangeOverlayFrequency;27;0;Create;True;0;0;0;False;0;False;0;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;561;3257.5,-459.1546;Inherit;False;Property;_RingTexColorRangeOverlayOffset;RingTexColorRangeOverlayOffset;25;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;566;4132.886,-349.1005;Inherit;False;Property;_RingTexColorRangeOverlayStrength;RingTexColorRangeOverlayStrength;26;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;563;4591.283,-296.0425;Inherit;False;RingTexColorRangeOverlay;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;50;343.6049,-920.9107;Inherit;False;Property;_BackTexMoveToCenterSpeed;BackTexMoveToCenterSpeed;32;0;Create;True;0;0;0;False;0;False;0;0.57;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;49;368.1766,-633.8679;Inherit;False;Property;_BackTexRotationSpeed;BackTexRotationSpeed;31;0;Create;True;0;0;0;False;0;False;0;9.73;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;319;2240.382,-2863.503;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;318;2381.997,-2205.791;Inherit;True;Property;_RingTexture;RingTexture;9;0;Create;True;0;0;0;False;0;False;-1;None;09e50757037a3554c9c38b50e92584dd;True;0;False;black;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.BreakToComponentsNode;483;2752.657,-2303.388;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SimpleAddOpNode;484;2872.37,-2297.5;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;485;3028.132,-2288.589;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;532;4173.212,-2061.017;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;535;4331.55,-2064.402;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;514;4517.892,-2164.333;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;513;3860.011,-1933.454;Inherit;False;369;BackTextureColorRange;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;457;-456.118,-2087.086;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;425;-331.6449,-2220.342;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.PosVertexDataNode;428;-390.2453,-1927.186;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;429;-152.5391,-1864.914;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;465;-60.62122,-1363.952;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;348;1510.545,-2884.748;Inherit;False;432;Radial Wobble Offset;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;570;129.6375,-2243.891;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RotatorNode;320;2182.084,-2387.076;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;2;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;531;3785.536,-2020.886;Inherit;False;516;BackTexColorRangeOverlay;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;576;3588.246,-2104.32;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;502;902.3771,-519.0074;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;501;661.1934,-504.4855;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;-0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;504;422.7547,-483.0258;Inherit;False;Property;_BackTexScale;BackTexScale;30;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;38;1658.492,-874.3408;Inherit;True;Property;_BackTexture;BackTexture;28;0;Create;True;0;0;0;False;0;False;-1;bafe7b58e07ace3478379b4d275ca1db;def94179b19c95740a14a81e3096e40d;True;2;False;black;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TextureCoordinatesNode;378;1332.947,-866.7255;Inherit;False;1;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;2,2;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.PiNode;582;1108.395,-438.906;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;583;1327.698,-449.9325;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;503;1197.819,-623.3535;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0.5,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;377;987.9332,-881.3678;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;580;1474.078,-625.6244;Inherit;False;True;4;0;FLOAT3;0,0,1;False;1;FLOAT;0;False;2;FLOAT3;0.5,0.5,0.5;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;268;-1020.216,-1537.555;Inherit;False;Property;_RingSwingSpeed;RingSwingSpeed;15;0;Create;True;0;0;0;False;0;False;0;2;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;581;827.5117,-300.07;Inherit;False;Property;_BackTexFixedRotation;BackTexFixedRotation;36;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;394;2064.694,-2154.154;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;473;2104.164,-563.6375;Inherit;False;Property;_BackTexValueRemap;BackTexValueRemap;29;0;Create;True;0;0;0;False;0;False;0,0;0,3.5;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;464;-498.9105,-1401.459;Inherit;False;Property;_RingSwingExtraWobble;RingSwingExtraWobble;17;0;Create;True;0;0;0;False;0;False;0;0.279;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;360;1274.723,-3038.141;Inherit;False;Property;_RingTexScale;RingTexScale;12;0;Create;True;0;0;0;False;0;False;1;0.82;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;424;-183.242,-2246.795;Inherit;True;Property;_RingWobbleTex;RingWobbleTex;19;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;448;-920.9893,-2771.35;Inherit;False;Property;_RingWobbleOverlayFrequency;RingWobbleOverlayFrequency;23;0;Create;True;0;0;0;False;0;False;0;1.09;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;444;-919.0613,-2680.696;Inherit;False;Property;_RingWobbleOverlayOffset;RingWobbleOverlayOffset;22;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;497;58.79853,-2563.37;Inherit;False;Property;_RingWobbleOverlayStrength;RingWobbleOverlayStrength;21;0;Create;True;0;0;0;False;0;False;0;1.274;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;322;1342.191,-2325.309;Inherit;False;Property;_RingTexRotation;RingTexRotation;13;0;Create;True;0;0;0;False;0;False;0;1.69;-3;3;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;427;-898.5199,-2137.362;Inherit;False;Property;_RingWobbleSpeed;RingWobbleSpeed;20;0;Create;True;0;0;0;False;0;False;0;0.279;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;450;-832.3965,-2007.45;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.IntNode;380;46.97411,-757.4567;Inherit;False;Property;_DynamicBackTecRotationSpeed;DynamicBackTecRotationSpeed;2;0;Create;True;0;0;0;False;0;False;1;1;False;0;1;INT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;569;251.1496,-2236.224;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;572;1127.891,-1585.509;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;584;992.5189,-1723.29;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.AbsOpNode;588;857.0254,-1799.964;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;530;5174.969,-2539.55;Inherit;False;573;RadialSwingColorRangeBoost;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;573;1308.685,-1658.175;Inherit;False;RadialSwingColorRangeBoost;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;534;3940.964,-2178.206;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;577;3779.453,-2257.76;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;571;873.5425,-1482.456;Inherit;False;Property;_RingSwingColorRangeBoost;RingSwingColorRangeBoost;24;0;Create;True;0;0;0;False;0;False;0;0.77;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;419;5286.466,-2469.434;Inherit;False;435;WobbleRadiusMulti;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;449;563.5823,-2154.669;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;435;483.4099,-2489.072;Inherit;False;WobbleRadiusMulti;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;432;1096.841,-2085.142;Inherit;False;Radial Wobble Offset;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;458;973.4294,-2086.11;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;586;801.957,-2151.988;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;426;353.7231,-2069.906;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.AbsOpNode;587;838.9653,-1703.989;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;585;810.5695,-2026.885;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;434;-570.7148,-1612.694;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;453;-419.1515,-1665.537;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;568;231.3088,-1577.469;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;567;94.50217,-1604.436;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;452;-244.044,-1644.037;Inherit;True;Property;_RingSwingTex;RingSwingTex;14;0;Create;True;0;0;0;False;0;False;-1;None;264241ef8deb3464abcd1ef777458a09;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;459;378.039,-1733.863;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;589;564.6313,-1892.443;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;433;31.53173,-1736.961;Inherit;False;Property;_RingSwingStrength;RingSwingStrength;16;0;Create;True;0;0;0;False;0;False;0;0.065;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;236;36.81185,-2015.868;Inherit;False;Property;_RingWobbleStrength;RingWobbleStrength;18;0;Create;True;0;0;0;False;0;False;0;0.08;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;221;-2676.811,-1911.063;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;223;-2483.936,-1945.756;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;222;-2486.343,-2104.077;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;226;-2274.735,-2054.011;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;117;-2601.2,-1674.143;Inherit;True;2;0;FLOAT2;0,1;False;1;FLOAT2;0,1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;254;-2144.551,-1604.372;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalizeNode;120;-2830.977,-1777.484;Inherit;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ATan2OpNode;225;-2045.943,-1852.979;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;253;-2126.253,-1701.049;Inherit;False;1;0;FLOAT;-1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;252;-1868.283,-1717.597;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;204;-1659.488,-1718.449;Inherit;False;angle;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;421;-2871.026,-1668.308;Inherit;False;Constant;_SeamVector;SeamVector;33;0;Create;True;0;0;0;False;0;False;0,1;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.PosVertexDataNode;203;-3342.449,-1853.062;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;56;-3110.821,-1813.263;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Compare;591;5275.803,-2115.028;Inherit;False;3;4;0;FLOAT;0;False;1;FLOAT;0.1;False;2;FLOAT;1;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;84;5408.448,-2327.232;Float;False;True;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;FusionRingVFXNew;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;True;True;1;5;False;;10;False;;2;5;False;;10;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;2;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;;0;0;Standard;23;Surface;1;638457958538089060;  Blend;0;0;Two Sided;1;0;Forward Only;0;0;Cast Shadows;1;0;  Use Shadow Threshold;0;0;Receive Shadows;1;0;GPU Instancing;1;0;LOD CrossFade;1;0;Built-in Fog;1;0;DOTS Instancing;0;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Vertex Position,InvertActionOnDeselection;1;0;0;10;False;True;True;True;False;False;True;True;True;False;False;;False;0
Node;AmplifyShaderEditor.DotProductOpNode;590;5113.803,-2217.028;Inherit;False;2;0;COLOR;0,0,0,0;False;1;COLOR;1,1,1,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;415;4770.914,-2275.438;Inherit;False;Property;_ShowMaster;ShowMaster;1;0;Create;True;0;0;0;False;0;False;1;0.3950119;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;592;4928.803,-2093.028;Inherit;False;Property;_Float0;Float 0;0;0;Create;True;0;0;0;False;0;False;1;0.3950119;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;544;4771.35,-4456.527;Inherit;False;543;ColorRangeFinal;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;539;5127.438,-4134.998;Inherit;False;4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.Compare;545;4834.554,-4041.198;Inherit;False;4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.Compare;548;4183.793,-3575.278;Inherit;False;4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;547;4625.079,-3944.732;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SaturateNode;487;4475.653,-3803.349;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;481;4279.98,-3859.479;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;550;3881.279,-3586.405;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SaturateNode;551;3659.645,-3571.033;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;552;3452.645,-3639.906;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;486;4672.028,-4351.989;Inherit;False;Property;_Color_2_Start;Color_2_Start;6;0;Create;True;0;0;0;False;0;False;0.3752641;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;549;3423.581,-3895.288;Inherit;False;543;ColorRangeFinal;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;546;4143.534,-4192.698;Inherit;False;543;ColorRangeFinal;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;542;3568.159,-3399.088;Inherit;False;Property;_Color3;Color 3;5;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,1.190095,12.62823,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;478;3466.304,-3767.421;Inherit;False;Property;_Color_3_Full;Color_3_Full;8;0;Create;True;0;0;0;False;0;False;0.3752641;0.914;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;477;3963.667,-3956.063;Inherit;False;Property;_Color_2;Color_2;4;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0.3266658,3.670186,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;474;4380.523,-4346.877;Inherit;False;Property;_Color_1;Color_1;3;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;493;3993.025,-4069.014;Inherit;False;Property;_Color_3_Start;Color_3_Start;7;0;Create;True;0;0;0;False;0;False;0;0.577;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;575;3201.716,-1955.027;Inherit;False;573;RadialSwingColorRangeBoost;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;472;2979.726,-2448.233;Inherit;False;Property;_RingTexValueRemap;RingTexValueRemap;10;0;Create;True;0;0;0;False;0;False;0,0;0.2,1.48;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.TFHCRemapNode;533;3369.525,-2323.383;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
WireConnection;324;0;322;0
WireConnection;324;1;325;0
WireConnection;324;2;323;0
WireConnection;376;0;50;0
WireConnection;376;1;379;0
WireConnection;375;0;379;0
WireConnection;375;1;49;0
WireConnection;417;0;50;0
WireConnection;417;1;376;0
WireConnection;418;0;375;0
WireConnection;418;1;49;0
WireConnection;379;0;380;0
WireConnection;361;0;360;0
WireConnection;362;0;361;0
WireConnection;494;0;362;0
WireConnection;494;1;348;0
WireConnection;494;2;496;0
WireConnection;509;0;38;1
WireConnection;509;1;38;2
WireConnection;509;2;38;3
WireConnection;508;0;509;0
WireConnection;512;0;511;0
WireConnection;511;0;508;0
WireConnection;511;1;473;1
WireConnection;511;2;473;2
WireConnection;369;0;512;0
WireConnection;342;0;341;1
WireConnection;342;1;341;3
WireConnection;344;0;343;0
WireConnection;344;1;340;0
WireConnection;343;0;342;0
WireConnection;463;0;268;0
WireConnection;463;1;460;0
WireConnection;430;0;429;0
WireConnection;455;0;427;0
WireConnection;455;1;450;0
WireConnection;345;0;344;0
WireConnection;460;0;268;0
WireConnection;460;1;461;0
WireConnection;437;0;445;0
WireConnection;437;2;497;0
WireConnection;445;0;436;0
WireConnection;441;0;438;0
WireConnection;447;0;443;0
WireConnection;447;1;448;0
WireConnection;438;0;447;0
WireConnection;438;1;446;0
WireConnection;446;0;444;0
WireConnection;436;0;441;0
WireConnection;404;0;539;0
WireConnection;404;1;415;0
WireConnection;538;0;514;0
WireConnection;543;0;538;0
WireConnection;518;0;524;0
WireConnection;520;0;523;0
WireConnection;520;1;527;0
WireConnection;521;0;520;0
WireConnection;521;1;522;0
WireConnection;522;0;526;0
WireConnection;524;0;519;0
WireConnection;517;0;518;0
WireConnection;517;2;525;0
WireConnection;516;0;517;0
WireConnection;519;0;521;0
WireConnection;555;0;559;0
WireConnection;556;0;560;0
WireConnection;556;1;564;0
WireConnection;557;0;556;0
WireConnection;557;1;558;0
WireConnection;558;0;561;0
WireConnection;559;0;565;0
WireConnection;562;0;555;0
WireConnection;562;2;566;0
WireConnection;565;0;557;0
WireConnection;563;0;562;0
WireConnection;319;0;360;0
WireConnection;319;1;494;0
WireConnection;318;1;320;0
WireConnection;483;0;318;0
WireConnection;484;0;483;0
WireConnection;484;1;483;1
WireConnection;484;2;483;2
WireConnection;485;0;484;0
WireConnection;532;0;531;0
WireConnection;532;1;513;0
WireConnection;535;0;532;0
WireConnection;514;0;534;0
WireConnection;514;1;535;0
WireConnection;457;0;427;0
WireConnection;457;1;455;0
WireConnection;425;0;423;0
WireConnection;425;1;457;0
WireConnection;429;0;428;1
WireConnection;429;1;428;3
WireConnection;465;0;464;0
WireConnection;465;1;461;0
WireConnection;570;0;424;1
WireConnection;570;1;424;2
WireConnection;570;2;424;3
WireConnection;320;0;319;0
WireConnection;320;2;394;0
WireConnection;576;0;533;0
WireConnection;576;1;575;0
WireConnection;502;0;501;0
WireConnection;501;0;504;0
WireConnection;38;1;580;0
WireConnection;378;0;504;0
WireConnection;378;1;503;0
WireConnection;582;0;581;0
WireConnection;583;0;582;0
WireConnection;503;0;502;0
WireConnection;503;1;377;0
WireConnection;377;0;418;0
WireConnection;377;1;417;0
WireConnection;580;1;583;0
WireConnection;580;3;378;0
WireConnection;394;0;324;0
WireConnection;424;1;425;0
WireConnection;569;0;570;0
WireConnection;572;0;584;0
WireConnection;572;1;571;0
WireConnection;584;0;588;0
WireConnection;584;1;587;0
WireConnection;588;0;589;0
WireConnection;573;0;572;0
WireConnection;534;0;577;0
WireConnection;577;0;533;0
WireConnection;577;1;576;0
WireConnection;449;0;435;0
WireConnection;449;1;426;0
WireConnection;435;0;437;0
WireConnection;432;0;458;0
WireConnection;458;0;586;0
WireConnection;458;1;585;0
WireConnection;586;0;449;0
WireConnection;586;1;430;0
WireConnection;426;0;569;0
WireConnection;426;1;236;0
WireConnection;587;0;459;0
WireConnection;585;0;430;0
WireConnection;585;1;589;0
WireConnection;434;0;456;0
WireConnection;434;1;463;0
WireConnection;453;0;434;0
WireConnection;453;1;465;0
WireConnection;568;0;567;0
WireConnection;567;0;452;1
WireConnection;567;1;452;2
WireConnection;567;2;452;3
WireConnection;452;1;453;0
WireConnection;459;0;433;0
WireConnection;459;1;568;0
WireConnection;589;0;435;0
WireConnection;589;1;459;0
WireConnection;221;0;120;0
WireConnection;223;0;221;1
WireConnection;223;1;421;1
WireConnection;222;0;221;0
WireConnection;222;2;421;2
WireConnection;226;0;222;0
WireConnection;226;1;223;0
WireConnection;117;0;120;0
WireConnection;117;1;421;0
WireConnection;120;0;56;0
WireConnection;225;0;226;0
WireConnection;225;1;117;0
WireConnection;252;0;225;0
WireConnection;252;1;253;0
WireConnection;252;2;254;0
WireConnection;204;0;252;0
WireConnection;56;0;203;1
WireConnection;56;1;203;3
WireConnection;591;0;590;0
WireConnection;591;1;592;0
WireConnection;84;2;404;0
WireConnection;84;3;591;0
WireConnection;590;0;404;0
WireConnection;539;0;544;0
WireConnection;539;1;486;0
WireConnection;539;2;474;0
WireConnection;539;3;545;0
WireConnection;545;0;546;0
WireConnection;545;1;493;0
WireConnection;545;2;547;0
WireConnection;545;3;548;0
WireConnection;548;0;549;0
WireConnection;548;1;478;0
WireConnection;548;2;550;0
WireConnection;548;3;542;0
WireConnection;547;0;474;0
WireConnection;547;1;477;0
WireConnection;547;2;487;0
WireConnection;487;0;481;0
WireConnection;481;0;546;0
WireConnection;481;1;486;0
WireConnection;481;2;493;0
WireConnection;550;0;477;0
WireConnection;550;1;542;0
WireConnection;550;2;551;0
WireConnection;551;0;552;0
WireConnection;552;0;549;0
WireConnection;552;1;493;0
WireConnection;552;2;478;0
WireConnection;533;0;485;0
WireConnection;533;1;472;1
WireConnection;533;2;472;2
ASEEND*/
//CHKSM=98BA61F48268FF28B6A74897CB464EDA9FF637B4