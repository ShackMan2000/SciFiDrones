// Made with Amplify Shader Editor v1.9.6.3
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Fire"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_PixelResolution("PixelResolution", Range( 1 , 100)) = 69.04025
		_RandomSeed("RandomSeed", Range( 1 , 100)) = 7.494699
		_StartPoint("StartPoint", Range( -1 , 1)) = 0.6464933
		_MidPoint("MidPoint", Range( -1 , 1)) = 0.4471616
		_EndPoint("EndPoint", Range( 0 , 10)) = 0.9124535
		_Radius("Radius", Range( 0 , 0.3)) = 0.1896872
		_FrontRoundness("FrontRoundness", Range( 0.1 , 100)) = 1
		_TailRoundness("TailRoundness", Range( 0.1 , 2)) = 1
		_VisZoneDark("VisZoneDark", Range( 0 , 0.3)) = 0
		_VisZoneMid("VisZoneMid", Range( 0 , 0.3)) = 0.3
		_VisZoneHigh("VisZoneHigh", Range( 0 , 0.3)) = 0.3
		[HDR]_Color1("Color 1", Color) = (1,1,1,1)
		_Color1Pure("Color 1 Pure", Range( 0 , 1)) = 0
		_Color1FadeOut("Color 1 Fade Out", Range( 0 , 1)) = 0.5
		[HDR]_Color2("Color 2", Color) = (8.909804,0.7529412,0,0)
		_Color2Pure("Color 2 Pure", Range( 0 , 1)) = 1
		_Color2FadedOut("Color 2 Faded Out", Range( 0 , 1)) = 1
		[HDR]_Color3("Color 3", Color) = (0.4488788,0,0,0)
		_VoronoiVerticalSpeed("VoronoiVerticalSpeed", Range( 0 , 5)) = 0.5128818
		_VoronoiTwirlSpeed("VoronoiTwirlSpeed", Range( -6 , 6)) = 1
		_VoronoiScale("VoronoiScale", Range( 0 , 10)) = 4.600065
		_VoronoiStart("VoronoiStart", Range( 0 , 1)) = 0
		_VoronoiMid("VoronoiMid", Range( 0 , 1)) = 0.5
		_VoronoiEnd("VoronoiEnd", Range( 0 , 1)) = 1
		_AddVoronoiAmount("AddVoronoiAmount", Range( 0 , 1)) = 1
		_DEBUGShowVoronoi("DEBUGShowVoronoi", Range( 0 , 1)) = 0
		_NoiseStart("NoiseStart", Range( 0 , 1)) = 0
		_NoiseMid("NoiseMid", Range( 0 , 1)) = 0.5
		_NoiseEnd("NoiseEnd", Range( 0 , 1)) = 1
		_NoiseScale("NoiseScale", Range( 0 , 20)) = 3.328468
		_NoiseScrollSpeed("NoiseScrollSpeed", Range( 0 , 5)) = 0.1074575
		_AddNoiseAmount("AddNoiseAmount", Range( 0 , 1)) = 1
		_DEBUGShowNoise("DEBUGShowNoise", Range( 0 , 1)) = 0
		_MasterAlpha("MasterAlpha", Range( 0 , 1)) = 1
		_AlphaStart("AlphaStart", Range( 0 , 1)) = 0
		_AlphaMid("AlphaMid", Range( 0 , 1)) = 0.5
		_AlphaHigh("AlphaHigh", Range( 0 , 1)) = 1
		_VerticalGradientPostProcess("VerticalGradientPostProcess", Range( 0 , 2)) = 0
		_TrailWobbleStrength("TrailWobbleStrength", Range( 0 , 5)) = 0
		_TrailWobbleScale("TrailWobbleScale", Range( 0 , 10)) = 0
		_TrailWobbleSpeed("TrailWobbleSpeed", Range( 0 , 5)) = 0
		_FinalMaskPower("FinalMaskPower", Range( 0 , 3)) = 1

		[HideInInspector][NoScaleOffset] unity_Lightmaps("unity_Lightmaps", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_LightmapsInd("unity_LightmapsInd", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_ShadowMasks("unity_ShadowMasks", 2DArray) = "" {}
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" }

		Cull Off
		HLSLINCLUDE
		#pragma target 2.0
		#pragma prefer_hlslcc gles
		// ensure rendering platforms toggle list is visible

		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Common.hlsl"
		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Filtering.hlsl"
		ENDHLSL

		
		Pass
		{
			Name "Sprite Unlit"
			Tags { "LightMode"="Universal2D" }

			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZTest LEqual
			ZWrite Off
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM

			#define ASE_SRP_VERSION 120103


			#pragma vertex vert
			#pragma fragment frag

			#define _SURFACE_TYPE_TRANSPARENT 1
			#define SHADERPASS SHADERPASS_SPRITEUNLIT

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#include "Packages/com.unity.render-pipelines.universal/Shaders/2D/Include/SurfaceData2D.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Debug/Debugging2D.hlsl"

			

			CBUFFER_START( UnityPerMaterial )
			float4 _Color1;
			float4 _Color3;
			float4 _Color2;
			float _TrailWobbleStrength;
			float _VoronoiMid;
			float _VoronoiStart;
			float _VoronoiEnd;
			float _AddVoronoiAmount;
			float _FinalMaskPower;
			float _Color1FadeOut;
			float _Color1Pure;
			float _Color2Pure;
			float _Color2FadedOut;
			float _VerticalGradientPostProcess;
			float _AlphaMid;
			float _AlphaStart;
			float _AlphaHigh;
			float _MasterAlpha;
			float _VoronoiScale;
			float _VoronoiTwirlSpeed;
			float _VoronoiVerticalSpeed;
			float _NoiseEnd;
			float _PixelResolution;
			float _StartPoint;
			float _MidPoint;
			float _FrontRoundness;
			float _EndPoint;
			float _TailRoundness;
			float _TrailWobbleScale;
			float _TrailWobbleSpeed;
			float _DEBUGShowVoronoi;
			float _RandomSeed;
			float _VisZoneMid;
			float _VisZoneDark;
			float _VisZoneHigh;
			float _AddNoiseAmount;
			float _NoiseScrollSpeed;
			float _NoiseScale;
			float _NoiseMid;
			float _NoiseStart;
			float _Radius;
			float _DEBUGShowNoise;
			CBUFFER_END


			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normal : NORMAL;
				float4 tangent : TANGENT;
				float4 uv0 : TEXCOORD0;
				float4 color : COLOR;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				float4 texCoord0 : TEXCOORD0;
				float4 color : TEXCOORD1;
				float3 positionWS : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			#if ETC1_EXTERNAL_ALPHA
				TEXTURE2D( _AlphaTex ); SAMPLER( sampler_AlphaTex );
				float _EnableAlphaTexture;
			#endif

			float4 _RendererColor;

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
			
			inline float2 UnityVoronoiRandomVector( float2 UV, float offset )
			{
				float2x2 m = float2x2( 15.27, 47.63, 99.41, 89.98 );
				UV = frac( sin(mul(UV, m) ) * 46839.32 );
				return float2( sin(UV.y* +offset ) * 0.5 + 0.5, cos( UV.x* offset ) * 0.5 + 0.5 );
			}
			
			//x - Out y - Cells
			float3 UnityVoronoi( float2 UV, float AngleOffset, float CellDensity, inout float2 mr )
			{
				float2 g = floor( UV * CellDensity );
				float2 f = frac( UV * CellDensity );
				float t = 8.0;
				float3 res = float3( 8.0, 0.0, 0.0 );
			
				for( int y = -1; y <= 1; y++ )
				{
					for( int x = -1; x <= 1; x++ )
					{
						float2 lattice = float2( x, y );
						float2 offset = UnityVoronoiRandomVector( lattice + g, AngleOffset );
						float d = distance( lattice + offset, f );
			
						if( d < res.x )
						{
							mr = f - lattice - offset;
							res = float3( d, offset.x, offset.y );
						}
					}
				}
				return res;
			}
			

			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3( 0, 0, 0 );
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif
				v.normal = v.normal;
				v.tangent.xyz = v.tangent.xyz;

				VertexPositionInputs vertexInput = GetVertexPositionInputs( v.positionOS.xyz );

				o.texCoord0 = v.uv0;
				o.color = v.color;
				o.positionCS = vertexInput.positionCS;
				o.positionWS = vertexInput.positionWS;

				return o;
			}

			half4 frag( VertexOutput IN  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				float2 texCoord558 = IN.texCoord0.xy * float2( 1,1 ) + float2( 0,0 );
				float Pixel_Resolution199 = floor( _PixelResolution );
				float pixelWidth560 =  1.0f / Pixel_Resolution199;
				float pixelHeight560 = 1.0f / Pixel_Resolution199;
				half2 pixelateduv560 = half2((int)(( ( texCoord558 - float2( 0.5,0.5 ) ) * float2( 2,-2 ) ).x / pixelWidth560) * pixelWidth560, (int)(( ( texCoord558 - float2( 0.5,0.5 ) ) * float2( 2,-2 ) ).y / pixelHeight560) * pixelHeight560);
				float2 Pixelated_Vertex_Positions315 = pixelateduv560;
				float Z_Axis_0to1347 = (1.0 + (Pixelated_Vertex_Positions315.y - -1.0) * (0.0 - 1.0) / (1.0 - -1.0));
				float Vertical_Mask373 = ( ( 1.0 - pow( ( 1.0 - ( saturate( (0.0 + (Z_Axis_0to1347 - _StartPoint) * (1.0 - 0.0) / (_MidPoint - _StartPoint)) ) * step( Z_Axis_0to1347 , _MidPoint ) ) ) , _FrontRoundness ) ) + ( 1.0 - pow( ( 1.0 - ( saturate( (1.0 + (Z_Axis_0to1347 - _MidPoint) * (0.0 - 1.0) / (_EndPoint - _MidPoint)) ) * step( _MidPoint , Z_Axis_0to1347 ) ) ) , _TailRoundness ) ) );
				float2 break291 = Pixelated_Vertex_Positions315;
				float mulTime526 = _TimeParameters.x * -_TrailWobbleSpeed;
				float RandomSeed533 = ( _RandomSeed + ( IN.texCoord0.z * 100.0 ) );
				float RandomOffsetPixelated544 = ( floor( ( RandomSeed533 * Pixel_Resolution199 ) ) / Pixel_Resolution199 );
				float Radius310 = _Radius;
				float Z_Proximity323 = saturate( (0.0 + (( 1.0 - abs( ( ( _TrailWobbleStrength * ( 1.0 - Vertical_Mask373 ) * step( 1E-05 , Vertical_Mask373 ) * sin( ( _TrailWobbleScale * ( break291.y + mulTime526 + RandomOffsetPixelated544 ) ) ) ) + break291.x ) ) ) - 0.0) * (Radius310 - 0.0) / (1.0 - 0.0)) );
				float temp_output_10_0_g13 = ( Z_Proximity323 * Vertical_Mask373 );
				float temp_output_13_0_g13 = _VisZoneMid;
				float temp_output_12_0_g13 = _VisZoneDark;
				float temp_output_14_0_g13 = _VisZoneHigh;
				float VisibilityZone268 = ( temp_output_10_0_g13 <= temp_output_13_0_g13 ? saturate( (0.0 + (temp_output_10_0_g13 - temp_output_12_0_g13) * (0.5 - 0.0) / (temp_output_13_0_g13 - temp_output_12_0_g13)) ) : saturate( (0.5 + (temp_output_10_0_g13 - temp_output_13_0_g13) * (1.0 - 0.5) / (temp_output_14_0_g13 - temp_output_13_0_g13)) ) );
				float mulTime53 = _TimeParameters.x * ( _NoiseScrollSpeed * -1.0 );
				float2 appendResult54 = (float2(RandomOffsetPixelated544 , ( floor( ( Pixel_Resolution199 * mulTime53 ) ) / Pixel_Resolution199 )));
				float2 texCoord13 = IN.texCoord0.xy * float2( 1,1 ) + appendResult54;
				float pixelWidth229 =  1.0f / Pixel_Resolution199;
				float pixelHeight229 = 1.0f / Pixel_Resolution199;
				half2 pixelateduv229 = half2((int)(texCoord13.x / pixelWidth229) * pixelWidth229, (int)(texCoord13.y / pixelHeight229) * pixelHeight229);
				float simplePerlin2D11 = snoise( pixelateduv229*_NoiseScale );
				simplePerlin2D11 = simplePerlin2D11*0.5 + 0.5;
				float temp_output_10_0_g12 = simplePerlin2D11;
				float temp_output_13_0_g12 = _NoiseMid;
				float temp_output_12_0_g12 = _NoiseStart;
				float temp_output_14_0_g12 = _NoiseEnd;
				float Noise464 = ( temp_output_10_0_g12 <= temp_output_13_0_g12 ? saturate( (0.0 + (temp_output_10_0_g12 - temp_output_12_0_g12) * (0.5 - 0.0) / (temp_output_13_0_g12 - temp_output_12_0_g12)) ) : saturate( (0.5 + (temp_output_10_0_g12 - temp_output_13_0_g12) * (1.0 - 0.5) / (temp_output_14_0_g12 - temp_output_13_0_g12)) ) );
				float mulTime513 = _TimeParameters.x * ( _VoronoiVerticalSpeed * -1.0 );
				float2 appendResult515 = (float2(RandomOffsetPixelated544 , ( floor( ( Pixel_Resolution199 * mulTime513 ) ) / Pixel_Resolution199 )));
				float2 texCoord8 = IN.texCoord0.xy * float2( 1,1 ) + appendResult515;
				float mulTime41 = _TimeParameters.x * _VoronoiTwirlSpeed;
				float2 uv17 = 0;
				float3 unityVoronoy17 = UnityVoronoi(( floor( ( Pixel_Resolution199 * texCoord8 ) ) / Pixel_Resolution199 ),mulTime41,_VoronoiScale,uv17);
				float temp_output_10_0_g9 = unityVoronoy17.x;
				float temp_output_13_0_g9 = _VoronoiMid;
				float temp_output_12_0_g9 = _VoronoiStart;
				float temp_output_14_0_g9 = _VoronoiEnd;
				float Voronoi50 = ( temp_output_10_0_g9 <= temp_output_13_0_g9 ? saturate( (0.0 + (temp_output_10_0_g9 - temp_output_12_0_g9) * (0.5 - 0.0) / (temp_output_13_0_g9 - temp_output_12_0_g9)) ) : saturate( (0.5 + (temp_output_10_0_g9 - temp_output_13_0_g9) * (1.0 - 0.5) / (temp_output_14_0_g9 - temp_output_13_0_g9)) ) );
				float temp_output_252_0 = ( ( _AddNoiseAmount * Noise464 ) + ( Noise464 * Voronoi50 ) + ( Voronoi50 * _AddVoronoiAmount ) );
				float temp_output_549_0 = saturate( temp_output_252_0 );
				float All_Noises244 = temp_output_549_0;
				float FinalGrayScaleMask465 = pow( saturate( ( VisibilityZone268 * ( VisibilityZone268 + All_Noises244 ) ) ) , _FinalMaskPower );
				float3 lerpResult490 = lerp( _Color1.rgb , _Color2.rgb , saturate( (0.0 + (FinalGrayScaleMask465 - _Color1Pure) * (1.0 - 0.0) / (_Color1FadeOut - _Color1Pure)) ));
				float3 lerpResult497 = lerp( _Color2.rgb , _Color3.rgb , saturate( (0.0 + (FinalGrayScaleMask465 - _Color2Pure) * (1.0 - 0.0) / (_Color2FadedOut - _Color2Pure)) ));
				float3 Color473 = ( FinalGrayScaleMask465 <= _Color1FadeOut ? lerpResult490 : lerpResult497 );
				float temp_output_10_0_g14 = FinalGrayScaleMask465;
				float temp_output_13_0_g14 = _AlphaMid;
				float temp_output_12_0_g14 = _AlphaStart;
				float temp_output_14_0_g14 = _AlphaHigh;
				float AlphaFinal572 = ( ( temp_output_10_0_g14 <= temp_output_13_0_g14 ? saturate( (0.0 + (temp_output_10_0_g14 - temp_output_12_0_g14) * (0.5 - 0.0) / (temp_output_13_0_g14 - temp_output_12_0_g14)) ) : saturate( (0.5 + (temp_output_10_0_g14 - temp_output_13_0_g14) * (1.0 - 0.5) / (temp_output_14_0_g14 - temp_output_13_0_g14)) ) ) * _MasterAlpha );
				float4 appendResult416 = (float4(( Color473 + ( (-4.0 + (Vertical_Mask373 - 0.0) * (1.0 - -4.0) / (1.0 - 0.0)) * _VerticalGradientPostProcess * Color473 ) ) , AlphaFinal572));
				float4 temp_cast_0 = (Voronoi50).xxxx;
				float4 lerpResult469 = lerp( appendResult416 , temp_cast_0 , _DEBUGShowVoronoi);
				float4 temp_cast_1 = (Noise464).xxxx;
				float4 lerpResult470 = lerp( lerpResult469 , temp_cast_1 , _DEBUGShowNoise);
				
				float4 Color = lerpResult470;

				#if ETC1_EXTERNAL_ALPHA
					float4 alpha = SAMPLE_TEXTURE2D( _AlphaTex, sampler_AlphaTex, IN.texCoord0.xy );
					Color.a = lerp( Color.a, alpha.r, _EnableAlphaTexture );
				#endif

				#if defined(DEBUG_DISPLAY)
				SurfaceData2D surfaceData;
				InitializeSurfaceData(Color.rgb, Color.a, surfaceData);
				InputData2D inputData;
				InitializeInputData(IN.positionWS.xy, half2(IN.texCoord0.xy), inputData);
				half4 debugColor = 0;

				SETUP_DEBUG_DATA_2D(inputData, IN.positionWS);

				if (CanDebugOverrideOutputColor(surfaceData, inputData, debugColor))
				{
					return debugColor;
				}
				#endif

				Color *= IN.color * _RendererColor;
				return Color;
			}

			ENDHLSL
		}
		
		Pass
		{
			
			Name "Sprite Unlit Forward"
            Tags { "LightMode"="UniversalForward" }

			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZTest LEqual
			ZWrite Off
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM

			#define ASE_SRP_VERSION 120103


			#pragma vertex vert
			#pragma fragment frag

			#define _SURFACE_TYPE_TRANSPARENT 1
			#define SHADERPASS SHADERPASS_SPRITEFORWARD

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#include "Packages/com.unity.render-pipelines.universal/Shaders/2D/Include/SurfaceData2D.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Debug/Debugging2D.hlsl"

			

			CBUFFER_START( UnityPerMaterial )
			float4 _Color1;
			float4 _Color3;
			float4 _Color2;
			float _TrailWobbleStrength;
			float _VoronoiMid;
			float _VoronoiStart;
			float _VoronoiEnd;
			float _AddVoronoiAmount;
			float _FinalMaskPower;
			float _Color1FadeOut;
			float _Color1Pure;
			float _Color2Pure;
			float _Color2FadedOut;
			float _VerticalGradientPostProcess;
			float _AlphaMid;
			float _AlphaStart;
			float _AlphaHigh;
			float _MasterAlpha;
			float _VoronoiScale;
			float _VoronoiTwirlSpeed;
			float _VoronoiVerticalSpeed;
			float _NoiseEnd;
			float _PixelResolution;
			float _StartPoint;
			float _MidPoint;
			float _FrontRoundness;
			float _EndPoint;
			float _TailRoundness;
			float _TrailWobbleScale;
			float _TrailWobbleSpeed;
			float _DEBUGShowVoronoi;
			float _RandomSeed;
			float _VisZoneMid;
			float _VisZoneDark;
			float _VisZoneHigh;
			float _AddNoiseAmount;
			float _NoiseScrollSpeed;
			float _NoiseScale;
			float _NoiseMid;
			float _NoiseStart;
			float _Radius;
			float _DEBUGShowNoise;
			CBUFFER_END


			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normal : NORMAL;
				float4 tangent : TANGENT;
				float4 uv0 : TEXCOORD0;
				float4 color : COLOR;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				float4 texCoord0 : TEXCOORD0;
				float4 color : TEXCOORD1;
				float3 positionWS : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			#if ETC1_EXTERNAL_ALPHA
				TEXTURE2D( _AlphaTex ); SAMPLER( sampler_AlphaTex );
				float _EnableAlphaTexture;
			#endif

			float4 _RendererColor;

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
			
			inline float2 UnityVoronoiRandomVector( float2 UV, float offset )
			{
				float2x2 m = float2x2( 15.27, 47.63, 99.41, 89.98 );
				UV = frac( sin(mul(UV, m) ) * 46839.32 );
				return float2( sin(UV.y* +offset ) * 0.5 + 0.5, cos( UV.x* offset ) * 0.5 + 0.5 );
			}
			
			//x - Out y - Cells
			float3 UnityVoronoi( float2 UV, float AngleOffset, float CellDensity, inout float2 mr )
			{
				float2 g = floor( UV * CellDensity );
				float2 f = frac( UV * CellDensity );
				float t = 8.0;
				float3 res = float3( 8.0, 0.0, 0.0 );
			
				for( int y = -1; y <= 1; y++ )
				{
					for( int x = -1; x <= 1; x++ )
					{
						float2 lattice = float2( x, y );
						float2 offset = UnityVoronoiRandomVector( lattice + g, AngleOffset );
						float d = distance( lattice + offset, f );
			
						if( d < res.x )
						{
							mr = f - lattice - offset;
							res = float3( d, offset.x, offset.y );
						}
					}
				}
				return res;
			}
			

			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3( 0, 0, 0 );
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif
				v.normal = v.normal;
				v.tangent.xyz = v.tangent.xyz;

				VertexPositionInputs vertexInput = GetVertexPositionInputs( v.positionOS.xyz );

				o.texCoord0 = v.uv0;
				o.color = v.color;
				o.positionCS = vertexInput.positionCS;
				o.positionWS = vertexInput.positionWS;

				return o;
			}

			half4 frag( VertexOutput IN  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				float2 texCoord558 = IN.texCoord0.xy * float2( 1,1 ) + float2( 0,0 );
				float Pixel_Resolution199 = floor( _PixelResolution );
				float pixelWidth560 =  1.0f / Pixel_Resolution199;
				float pixelHeight560 = 1.0f / Pixel_Resolution199;
				half2 pixelateduv560 = half2((int)(( ( texCoord558 - float2( 0.5,0.5 ) ) * float2( 2,-2 ) ).x / pixelWidth560) * pixelWidth560, (int)(( ( texCoord558 - float2( 0.5,0.5 ) ) * float2( 2,-2 ) ).y / pixelHeight560) * pixelHeight560);
				float2 Pixelated_Vertex_Positions315 = pixelateduv560;
				float Z_Axis_0to1347 = (1.0 + (Pixelated_Vertex_Positions315.y - -1.0) * (0.0 - 1.0) / (1.0 - -1.0));
				float Vertical_Mask373 = ( ( 1.0 - pow( ( 1.0 - ( saturate( (0.0 + (Z_Axis_0to1347 - _StartPoint) * (1.0 - 0.0) / (_MidPoint - _StartPoint)) ) * step( Z_Axis_0to1347 , _MidPoint ) ) ) , _FrontRoundness ) ) + ( 1.0 - pow( ( 1.0 - ( saturate( (1.0 + (Z_Axis_0to1347 - _MidPoint) * (0.0 - 1.0) / (_EndPoint - _MidPoint)) ) * step( _MidPoint , Z_Axis_0to1347 ) ) ) , _TailRoundness ) ) );
				float2 break291 = Pixelated_Vertex_Positions315;
				float mulTime526 = _TimeParameters.x * -_TrailWobbleSpeed;
				float RandomSeed533 = ( _RandomSeed + ( IN.texCoord0.z * 100.0 ) );
				float RandomOffsetPixelated544 = ( floor( ( RandomSeed533 * Pixel_Resolution199 ) ) / Pixel_Resolution199 );
				float Radius310 = _Radius;
				float Z_Proximity323 = saturate( (0.0 + (( 1.0 - abs( ( ( _TrailWobbleStrength * ( 1.0 - Vertical_Mask373 ) * step( 1E-05 , Vertical_Mask373 ) * sin( ( _TrailWobbleScale * ( break291.y + mulTime526 + RandomOffsetPixelated544 ) ) ) ) + break291.x ) ) ) - 0.0) * (Radius310 - 0.0) / (1.0 - 0.0)) );
				float temp_output_10_0_g13 = ( Z_Proximity323 * Vertical_Mask373 );
				float temp_output_13_0_g13 = _VisZoneMid;
				float temp_output_12_0_g13 = _VisZoneDark;
				float temp_output_14_0_g13 = _VisZoneHigh;
				float VisibilityZone268 = ( temp_output_10_0_g13 <= temp_output_13_0_g13 ? saturate( (0.0 + (temp_output_10_0_g13 - temp_output_12_0_g13) * (0.5 - 0.0) / (temp_output_13_0_g13 - temp_output_12_0_g13)) ) : saturate( (0.5 + (temp_output_10_0_g13 - temp_output_13_0_g13) * (1.0 - 0.5) / (temp_output_14_0_g13 - temp_output_13_0_g13)) ) );
				float mulTime53 = _TimeParameters.x * ( _NoiseScrollSpeed * -1.0 );
				float2 appendResult54 = (float2(RandomOffsetPixelated544 , ( floor( ( Pixel_Resolution199 * mulTime53 ) ) / Pixel_Resolution199 )));
				float2 texCoord13 = IN.texCoord0.xy * float2( 1,1 ) + appendResult54;
				float pixelWidth229 =  1.0f / Pixel_Resolution199;
				float pixelHeight229 = 1.0f / Pixel_Resolution199;
				half2 pixelateduv229 = half2((int)(texCoord13.x / pixelWidth229) * pixelWidth229, (int)(texCoord13.y / pixelHeight229) * pixelHeight229);
				float simplePerlin2D11 = snoise( pixelateduv229*_NoiseScale );
				simplePerlin2D11 = simplePerlin2D11*0.5 + 0.5;
				float temp_output_10_0_g12 = simplePerlin2D11;
				float temp_output_13_0_g12 = _NoiseMid;
				float temp_output_12_0_g12 = _NoiseStart;
				float temp_output_14_0_g12 = _NoiseEnd;
				float Noise464 = ( temp_output_10_0_g12 <= temp_output_13_0_g12 ? saturate( (0.0 + (temp_output_10_0_g12 - temp_output_12_0_g12) * (0.5 - 0.0) / (temp_output_13_0_g12 - temp_output_12_0_g12)) ) : saturate( (0.5 + (temp_output_10_0_g12 - temp_output_13_0_g12) * (1.0 - 0.5) / (temp_output_14_0_g12 - temp_output_13_0_g12)) ) );
				float mulTime513 = _TimeParameters.x * ( _VoronoiVerticalSpeed * -1.0 );
				float2 appendResult515 = (float2(RandomOffsetPixelated544 , ( floor( ( Pixel_Resolution199 * mulTime513 ) ) / Pixel_Resolution199 )));
				float2 texCoord8 = IN.texCoord0.xy * float2( 1,1 ) + appendResult515;
				float mulTime41 = _TimeParameters.x * _VoronoiTwirlSpeed;
				float2 uv17 = 0;
				float3 unityVoronoy17 = UnityVoronoi(( floor( ( Pixel_Resolution199 * texCoord8 ) ) / Pixel_Resolution199 ),mulTime41,_VoronoiScale,uv17);
				float temp_output_10_0_g9 = unityVoronoy17.x;
				float temp_output_13_0_g9 = _VoronoiMid;
				float temp_output_12_0_g9 = _VoronoiStart;
				float temp_output_14_0_g9 = _VoronoiEnd;
				float Voronoi50 = ( temp_output_10_0_g9 <= temp_output_13_0_g9 ? saturate( (0.0 + (temp_output_10_0_g9 - temp_output_12_0_g9) * (0.5 - 0.0) / (temp_output_13_0_g9 - temp_output_12_0_g9)) ) : saturate( (0.5 + (temp_output_10_0_g9 - temp_output_13_0_g9) * (1.0 - 0.5) / (temp_output_14_0_g9 - temp_output_13_0_g9)) ) );
				float temp_output_252_0 = ( ( _AddNoiseAmount * Noise464 ) + ( Noise464 * Voronoi50 ) + ( Voronoi50 * _AddVoronoiAmount ) );
				float temp_output_549_0 = saturate( temp_output_252_0 );
				float All_Noises244 = temp_output_549_0;
				float FinalGrayScaleMask465 = pow( saturate( ( VisibilityZone268 * ( VisibilityZone268 + All_Noises244 ) ) ) , _FinalMaskPower );
				float3 lerpResult490 = lerp( _Color1.rgb , _Color2.rgb , saturate( (0.0 + (FinalGrayScaleMask465 - _Color1Pure) * (1.0 - 0.0) / (_Color1FadeOut - _Color1Pure)) ));
				float3 lerpResult497 = lerp( _Color2.rgb , _Color3.rgb , saturate( (0.0 + (FinalGrayScaleMask465 - _Color2Pure) * (1.0 - 0.0) / (_Color2FadedOut - _Color2Pure)) ));
				float3 Color473 = ( FinalGrayScaleMask465 <= _Color1FadeOut ? lerpResult490 : lerpResult497 );
				float temp_output_10_0_g14 = FinalGrayScaleMask465;
				float temp_output_13_0_g14 = _AlphaMid;
				float temp_output_12_0_g14 = _AlphaStart;
				float temp_output_14_0_g14 = _AlphaHigh;
				float AlphaFinal572 = ( ( temp_output_10_0_g14 <= temp_output_13_0_g14 ? saturate( (0.0 + (temp_output_10_0_g14 - temp_output_12_0_g14) * (0.5 - 0.0) / (temp_output_13_0_g14 - temp_output_12_0_g14)) ) : saturate( (0.5 + (temp_output_10_0_g14 - temp_output_13_0_g14) * (1.0 - 0.5) / (temp_output_14_0_g14 - temp_output_13_0_g14)) ) ) * _MasterAlpha );
				float4 appendResult416 = (float4(( Color473 + ( (-4.0 + (Vertical_Mask373 - 0.0) * (1.0 - -4.0) / (1.0 - 0.0)) * _VerticalGradientPostProcess * Color473 ) ) , AlphaFinal572));
				float4 temp_cast_0 = (Voronoi50).xxxx;
				float4 lerpResult469 = lerp( appendResult416 , temp_cast_0 , _DEBUGShowVoronoi);
				float4 temp_cast_1 = (Noise464).xxxx;
				float4 lerpResult470 = lerp( lerpResult469 , temp_cast_1 , _DEBUGShowNoise);
				
				float4 Color = lerpResult470;

				#if ETC1_EXTERNAL_ALPHA
					float4 alpha = SAMPLE_TEXTURE2D( _AlphaTex, sampler_AlphaTex, IN.texCoord0.xy );
					Color.a = lerp( Color.a, alpha.r, _EnableAlphaTexture );
				#endif


				#if defined(DEBUG_DISPLAY)
				SurfaceData2D surfaceData;
				InitializeSurfaceData(Color.rgb, Color.a, surfaceData);
				InputData2D inputData;
				InitializeInputData(IN.positionWS.xy, half2(IN.texCoord0.xy), inputData);
				half4 debugColor = 0;

				SETUP_DEBUG_DATA_2D(inputData, IN.positionWS);

				if (CanDebugOverrideOutputColor(surfaceData, inputData, debugColor))
				{
					return debugColor;
				}
				#endif

				Color *= IN.color * _RendererColor;
				return Color;
			}

			ENDHLSL
		}
		
        Pass
        {
			
            Name "SceneSelectionPass"
            Tags { "LightMode"="SceneSelectionPass" }

            Cull Off

            HLSLPROGRAM

			#define ASE_SRP_VERSION 120103


			#pragma vertex vert
			#pragma fragment frag

            #define _SURFACE_TYPE_TRANSPARENT 1
            #define ATTRIBUTES_NEED_NORMAL
            #define ATTRIBUTES_NEED_TANGENT
            #define FEATURES_GRAPH_VERTEX
            #define SHADERPASS SHADERPASS_DEPTHONLY
			#define SCENESELECTIONPASS 1


            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			

			CBUFFER_START( UnityPerMaterial )
			float4 _Color1;
			float4 _Color3;
			float4 _Color2;
			float _TrailWobbleStrength;
			float _VoronoiMid;
			float _VoronoiStart;
			float _VoronoiEnd;
			float _AddVoronoiAmount;
			float _FinalMaskPower;
			float _Color1FadeOut;
			float _Color1Pure;
			float _Color2Pure;
			float _Color2FadedOut;
			float _VerticalGradientPostProcess;
			float _AlphaMid;
			float _AlphaStart;
			float _AlphaHigh;
			float _MasterAlpha;
			float _VoronoiScale;
			float _VoronoiTwirlSpeed;
			float _VoronoiVerticalSpeed;
			float _NoiseEnd;
			float _PixelResolution;
			float _StartPoint;
			float _MidPoint;
			float _FrontRoundness;
			float _EndPoint;
			float _TailRoundness;
			float _TrailWobbleScale;
			float _TrailWobbleSpeed;
			float _DEBUGShowVoronoi;
			float _RandomSeed;
			float _VisZoneMid;
			float _VisZoneDark;
			float _VisZoneHigh;
			float _AddNoiseAmount;
			float _NoiseScrollSpeed;
			float _NoiseScale;
			float _NoiseMid;
			float _NoiseStart;
			float _Radius;
			float _DEBUGShowNoise;
			CBUFFER_END


            struct VertexInput
			{
				float3 positionOS : POSITION;
				float3 normal : NORMAL;
				float4 tangent : TANGENT;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};


            int _ObjectId;
            int _PassValue;

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
			
			inline float2 UnityVoronoiRandomVector( float2 UV, float offset )
			{
				float2x2 m = float2x2( 15.27, 47.63, 99.41, 89.98 );
				UV = frac( sin(mul(UV, m) ) * 46839.32 );
				return float2( sin(UV.y* +offset ) * 0.5 + 0.5, cos( UV.x* offset ) * 0.5 + 0.5 );
			}
			
			//x - Out y - Cells
			float3 UnityVoronoi( float2 UV, float AngleOffset, float CellDensity, inout float2 mr )
			{
				float2 g = floor( UV * CellDensity );
				float2 f = frac( UV * CellDensity );
				float t = 8.0;
				float3 res = float3( 8.0, 0.0, 0.0 );
			
				for( int y = -1; y <= 1; y++ )
				{
					for( int x = -1; x <= 1; x++ )
					{
						float2 lattice = float2( x, y );
						float2 offset = UnityVoronoiRandomVector( lattice + g, AngleOffset );
						float d = distance( lattice + offset, f );
			
						if( d < res.x )
						{
							mr = f - lattice - offset;
							res = float3( d, offset.x, offset.y );
						}
					}
				}
				return res;
			}
			

			VertexOutput vert(VertexInput v )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				o.ase_texcoord = v.ase_texcoord;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif

				VertexPositionInputs vertexInput = GetVertexPositionInputs(v.positionOS.xyz);
				float3 positionWS = TransformObjectToWorld(v.positionOS);
				o.positionCS = TransformWorldToHClip(positionWS);

				return o;
			}

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				float2 texCoord558 = IN.ase_texcoord.xy * float2( 1,1 ) + float2( 0,0 );
				float Pixel_Resolution199 = floor( _PixelResolution );
				float pixelWidth560 =  1.0f / Pixel_Resolution199;
				float pixelHeight560 = 1.0f / Pixel_Resolution199;
				half2 pixelateduv560 = half2((int)(( ( texCoord558 - float2( 0.5,0.5 ) ) * float2( 2,-2 ) ).x / pixelWidth560) * pixelWidth560, (int)(( ( texCoord558 - float2( 0.5,0.5 ) ) * float2( 2,-2 ) ).y / pixelHeight560) * pixelHeight560);
				float2 Pixelated_Vertex_Positions315 = pixelateduv560;
				float Z_Axis_0to1347 = (1.0 + (Pixelated_Vertex_Positions315.y - -1.0) * (0.0 - 1.0) / (1.0 - -1.0));
				float Vertical_Mask373 = ( ( 1.0 - pow( ( 1.0 - ( saturate( (0.0 + (Z_Axis_0to1347 - _StartPoint) * (1.0 - 0.0) / (_MidPoint - _StartPoint)) ) * step( Z_Axis_0to1347 , _MidPoint ) ) ) , _FrontRoundness ) ) + ( 1.0 - pow( ( 1.0 - ( saturate( (1.0 + (Z_Axis_0to1347 - _MidPoint) * (0.0 - 1.0) / (_EndPoint - _MidPoint)) ) * step( _MidPoint , Z_Axis_0to1347 ) ) ) , _TailRoundness ) ) );
				float2 break291 = Pixelated_Vertex_Positions315;
				float mulTime526 = _TimeParameters.x * -_TrailWobbleSpeed;
				float RandomSeed533 = ( _RandomSeed + ( IN.ase_texcoord.z * 100.0 ) );
				float RandomOffsetPixelated544 = ( floor( ( RandomSeed533 * Pixel_Resolution199 ) ) / Pixel_Resolution199 );
				float Radius310 = _Radius;
				float Z_Proximity323 = saturate( (0.0 + (( 1.0 - abs( ( ( _TrailWobbleStrength * ( 1.0 - Vertical_Mask373 ) * step( 1E-05 , Vertical_Mask373 ) * sin( ( _TrailWobbleScale * ( break291.y + mulTime526 + RandomOffsetPixelated544 ) ) ) ) + break291.x ) ) ) - 0.0) * (Radius310 - 0.0) / (1.0 - 0.0)) );
				float temp_output_10_0_g13 = ( Z_Proximity323 * Vertical_Mask373 );
				float temp_output_13_0_g13 = _VisZoneMid;
				float temp_output_12_0_g13 = _VisZoneDark;
				float temp_output_14_0_g13 = _VisZoneHigh;
				float VisibilityZone268 = ( temp_output_10_0_g13 <= temp_output_13_0_g13 ? saturate( (0.0 + (temp_output_10_0_g13 - temp_output_12_0_g13) * (0.5 - 0.0) / (temp_output_13_0_g13 - temp_output_12_0_g13)) ) : saturate( (0.5 + (temp_output_10_0_g13 - temp_output_13_0_g13) * (1.0 - 0.5) / (temp_output_14_0_g13 - temp_output_13_0_g13)) ) );
				float mulTime53 = _TimeParameters.x * ( _NoiseScrollSpeed * -1.0 );
				float2 appendResult54 = (float2(RandomOffsetPixelated544 , ( floor( ( Pixel_Resolution199 * mulTime53 ) ) / Pixel_Resolution199 )));
				float2 texCoord13 = IN.ase_texcoord.xy * float2( 1,1 ) + appendResult54;
				float pixelWidth229 =  1.0f / Pixel_Resolution199;
				float pixelHeight229 = 1.0f / Pixel_Resolution199;
				half2 pixelateduv229 = half2((int)(texCoord13.x / pixelWidth229) * pixelWidth229, (int)(texCoord13.y / pixelHeight229) * pixelHeight229);
				float simplePerlin2D11 = snoise( pixelateduv229*_NoiseScale );
				simplePerlin2D11 = simplePerlin2D11*0.5 + 0.5;
				float temp_output_10_0_g12 = simplePerlin2D11;
				float temp_output_13_0_g12 = _NoiseMid;
				float temp_output_12_0_g12 = _NoiseStart;
				float temp_output_14_0_g12 = _NoiseEnd;
				float Noise464 = ( temp_output_10_0_g12 <= temp_output_13_0_g12 ? saturate( (0.0 + (temp_output_10_0_g12 - temp_output_12_0_g12) * (0.5 - 0.0) / (temp_output_13_0_g12 - temp_output_12_0_g12)) ) : saturate( (0.5 + (temp_output_10_0_g12 - temp_output_13_0_g12) * (1.0 - 0.5) / (temp_output_14_0_g12 - temp_output_13_0_g12)) ) );
				float mulTime513 = _TimeParameters.x * ( _VoronoiVerticalSpeed * -1.0 );
				float2 appendResult515 = (float2(RandomOffsetPixelated544 , ( floor( ( Pixel_Resolution199 * mulTime513 ) ) / Pixel_Resolution199 )));
				float2 texCoord8 = IN.ase_texcoord.xy * float2( 1,1 ) + appendResult515;
				float mulTime41 = _TimeParameters.x * _VoronoiTwirlSpeed;
				float2 uv17 = 0;
				float3 unityVoronoy17 = UnityVoronoi(( floor( ( Pixel_Resolution199 * texCoord8 ) ) / Pixel_Resolution199 ),mulTime41,_VoronoiScale,uv17);
				float temp_output_10_0_g9 = unityVoronoy17.x;
				float temp_output_13_0_g9 = _VoronoiMid;
				float temp_output_12_0_g9 = _VoronoiStart;
				float temp_output_14_0_g9 = _VoronoiEnd;
				float Voronoi50 = ( temp_output_10_0_g9 <= temp_output_13_0_g9 ? saturate( (0.0 + (temp_output_10_0_g9 - temp_output_12_0_g9) * (0.5 - 0.0) / (temp_output_13_0_g9 - temp_output_12_0_g9)) ) : saturate( (0.5 + (temp_output_10_0_g9 - temp_output_13_0_g9) * (1.0 - 0.5) / (temp_output_14_0_g9 - temp_output_13_0_g9)) ) );
				float temp_output_252_0 = ( ( _AddNoiseAmount * Noise464 ) + ( Noise464 * Voronoi50 ) + ( Voronoi50 * _AddVoronoiAmount ) );
				float temp_output_549_0 = saturate( temp_output_252_0 );
				float All_Noises244 = temp_output_549_0;
				float FinalGrayScaleMask465 = pow( saturate( ( VisibilityZone268 * ( VisibilityZone268 + All_Noises244 ) ) ) , _FinalMaskPower );
				float3 lerpResult490 = lerp( _Color1.rgb , _Color2.rgb , saturate( (0.0 + (FinalGrayScaleMask465 - _Color1Pure) * (1.0 - 0.0) / (_Color1FadeOut - _Color1Pure)) ));
				float3 lerpResult497 = lerp( _Color2.rgb , _Color3.rgb , saturate( (0.0 + (FinalGrayScaleMask465 - _Color2Pure) * (1.0 - 0.0) / (_Color2FadedOut - _Color2Pure)) ));
				float3 Color473 = ( FinalGrayScaleMask465 <= _Color1FadeOut ? lerpResult490 : lerpResult497 );
				float temp_output_10_0_g14 = FinalGrayScaleMask465;
				float temp_output_13_0_g14 = _AlphaMid;
				float temp_output_12_0_g14 = _AlphaStart;
				float temp_output_14_0_g14 = _AlphaHigh;
				float AlphaFinal572 = ( ( temp_output_10_0_g14 <= temp_output_13_0_g14 ? saturate( (0.0 + (temp_output_10_0_g14 - temp_output_12_0_g14) * (0.5 - 0.0) / (temp_output_13_0_g14 - temp_output_12_0_g14)) ) : saturate( (0.5 + (temp_output_10_0_g14 - temp_output_13_0_g14) * (1.0 - 0.5) / (temp_output_14_0_g14 - temp_output_13_0_g14)) ) ) * _MasterAlpha );
				float4 appendResult416 = (float4(( Color473 + ( (-4.0 + (Vertical_Mask373 - 0.0) * (1.0 - -4.0) / (1.0 - 0.0)) * _VerticalGradientPostProcess * Color473 ) ) , AlphaFinal572));
				float4 temp_cast_0 = (Voronoi50).xxxx;
				float4 lerpResult469 = lerp( appendResult416 , temp_cast_0 , _DEBUGShowVoronoi);
				float4 temp_cast_1 = (Noise464).xxxx;
				float4 lerpResult470 = lerp( lerpResult469 , temp_cast_1 , _DEBUGShowNoise);
				
				float4 Color = lerpResult470;

				half4 outColor = half4(_ObjectId, _PassValue, 1.0, 1.0);
				return outColor;
			}

            ENDHLSL
        }

		
        Pass
        {
			
            Name "ScenePickingPass"
            Tags { "LightMode"="Picking" }

            Cull Off

            HLSLPROGRAM

			#define ASE_SRP_VERSION 120103


			#pragma vertex vert
			#pragma fragment frag

            #define _SURFACE_TYPE_TRANSPARENT 1
            #define ATTRIBUTES_NEED_NORMAL
            #define ATTRIBUTES_NEED_TANGENT
            #define FEATURES_GRAPH_VERTEX
            #define SHADERPASS SHADERPASS_DEPTHONLY
			#define SCENEPICKINGPASS 1


            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

        	

			CBUFFER_START( UnityPerMaterial )
			float4 _Color1;
			float4 _Color3;
			float4 _Color2;
			float _TrailWobbleStrength;
			float _VoronoiMid;
			float _VoronoiStart;
			float _VoronoiEnd;
			float _AddVoronoiAmount;
			float _FinalMaskPower;
			float _Color1FadeOut;
			float _Color1Pure;
			float _Color2Pure;
			float _Color2FadedOut;
			float _VerticalGradientPostProcess;
			float _AlphaMid;
			float _AlphaStart;
			float _AlphaHigh;
			float _MasterAlpha;
			float _VoronoiScale;
			float _VoronoiTwirlSpeed;
			float _VoronoiVerticalSpeed;
			float _NoiseEnd;
			float _PixelResolution;
			float _StartPoint;
			float _MidPoint;
			float _FrontRoundness;
			float _EndPoint;
			float _TailRoundness;
			float _TrailWobbleScale;
			float _TrailWobbleSpeed;
			float _DEBUGShowVoronoi;
			float _RandomSeed;
			float _VisZoneMid;
			float _VisZoneDark;
			float _VisZoneHigh;
			float _AddNoiseAmount;
			float _NoiseScrollSpeed;
			float _NoiseScale;
			float _NoiseMid;
			float _NoiseStart;
			float _Radius;
			float _DEBUGShowNoise;
			CBUFFER_END


            struct VertexInput
			{
				float3 positionOS : POSITION;
				float3 normal : NORMAL;
				float4 tangent : TANGENT;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

            float4 _SelectionID;

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
			
			inline float2 UnityVoronoiRandomVector( float2 UV, float offset )
			{
				float2x2 m = float2x2( 15.27, 47.63, 99.41, 89.98 );
				UV = frac( sin(mul(UV, m) ) * 46839.32 );
				return float2( sin(UV.y* +offset ) * 0.5 + 0.5, cos( UV.x* offset ) * 0.5 + 0.5 );
			}
			
			//x - Out y - Cells
			float3 UnityVoronoi( float2 UV, float AngleOffset, float CellDensity, inout float2 mr )
			{
				float2 g = floor( UV * CellDensity );
				float2 f = frac( UV * CellDensity );
				float t = 8.0;
				float3 res = float3( 8.0, 0.0, 0.0 );
			
				for( int y = -1; y <= 1; y++ )
				{
					for( int x = -1; x <= 1; x++ )
					{
						float2 lattice = float2( x, y );
						float2 offset = UnityVoronoiRandomVector( lattice + g, AngleOffset );
						float d = distance( lattice + offset, f );
			
						if( d < res.x )
						{
							mr = f - lattice - offset;
							res = float3( d, offset.x, offset.y );
						}
					}
				}
				return res;
			}
			

			VertexOutput vert(VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				o.ase_texcoord = v.ase_texcoord;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif

				VertexPositionInputs vertexInput = GetVertexPositionInputs(v.positionOS.xyz);
				float3 positionWS = TransformObjectToWorld(v.positionOS);
				o.positionCS = TransformWorldToHClip(positionWS);

				return o;
			}

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				float2 texCoord558 = IN.ase_texcoord.xy * float2( 1,1 ) + float2( 0,0 );
				float Pixel_Resolution199 = floor( _PixelResolution );
				float pixelWidth560 =  1.0f / Pixel_Resolution199;
				float pixelHeight560 = 1.0f / Pixel_Resolution199;
				half2 pixelateduv560 = half2((int)(( ( texCoord558 - float2( 0.5,0.5 ) ) * float2( 2,-2 ) ).x / pixelWidth560) * pixelWidth560, (int)(( ( texCoord558 - float2( 0.5,0.5 ) ) * float2( 2,-2 ) ).y / pixelHeight560) * pixelHeight560);
				float2 Pixelated_Vertex_Positions315 = pixelateduv560;
				float Z_Axis_0to1347 = (1.0 + (Pixelated_Vertex_Positions315.y - -1.0) * (0.0 - 1.0) / (1.0 - -1.0));
				float Vertical_Mask373 = ( ( 1.0 - pow( ( 1.0 - ( saturate( (0.0 + (Z_Axis_0to1347 - _StartPoint) * (1.0 - 0.0) / (_MidPoint - _StartPoint)) ) * step( Z_Axis_0to1347 , _MidPoint ) ) ) , _FrontRoundness ) ) + ( 1.0 - pow( ( 1.0 - ( saturate( (1.0 + (Z_Axis_0to1347 - _MidPoint) * (0.0 - 1.0) / (_EndPoint - _MidPoint)) ) * step( _MidPoint , Z_Axis_0to1347 ) ) ) , _TailRoundness ) ) );
				float2 break291 = Pixelated_Vertex_Positions315;
				float mulTime526 = _TimeParameters.x * -_TrailWobbleSpeed;
				float RandomSeed533 = ( _RandomSeed + ( IN.ase_texcoord.z * 100.0 ) );
				float RandomOffsetPixelated544 = ( floor( ( RandomSeed533 * Pixel_Resolution199 ) ) / Pixel_Resolution199 );
				float Radius310 = _Radius;
				float Z_Proximity323 = saturate( (0.0 + (( 1.0 - abs( ( ( _TrailWobbleStrength * ( 1.0 - Vertical_Mask373 ) * step( 1E-05 , Vertical_Mask373 ) * sin( ( _TrailWobbleScale * ( break291.y + mulTime526 + RandomOffsetPixelated544 ) ) ) ) + break291.x ) ) ) - 0.0) * (Radius310 - 0.0) / (1.0 - 0.0)) );
				float temp_output_10_0_g13 = ( Z_Proximity323 * Vertical_Mask373 );
				float temp_output_13_0_g13 = _VisZoneMid;
				float temp_output_12_0_g13 = _VisZoneDark;
				float temp_output_14_0_g13 = _VisZoneHigh;
				float VisibilityZone268 = ( temp_output_10_0_g13 <= temp_output_13_0_g13 ? saturate( (0.0 + (temp_output_10_0_g13 - temp_output_12_0_g13) * (0.5 - 0.0) / (temp_output_13_0_g13 - temp_output_12_0_g13)) ) : saturate( (0.5 + (temp_output_10_0_g13 - temp_output_13_0_g13) * (1.0 - 0.5) / (temp_output_14_0_g13 - temp_output_13_0_g13)) ) );
				float mulTime53 = _TimeParameters.x * ( _NoiseScrollSpeed * -1.0 );
				float2 appendResult54 = (float2(RandomOffsetPixelated544 , ( floor( ( Pixel_Resolution199 * mulTime53 ) ) / Pixel_Resolution199 )));
				float2 texCoord13 = IN.ase_texcoord.xy * float2( 1,1 ) + appendResult54;
				float pixelWidth229 =  1.0f / Pixel_Resolution199;
				float pixelHeight229 = 1.0f / Pixel_Resolution199;
				half2 pixelateduv229 = half2((int)(texCoord13.x / pixelWidth229) * pixelWidth229, (int)(texCoord13.y / pixelHeight229) * pixelHeight229);
				float simplePerlin2D11 = snoise( pixelateduv229*_NoiseScale );
				simplePerlin2D11 = simplePerlin2D11*0.5 + 0.5;
				float temp_output_10_0_g12 = simplePerlin2D11;
				float temp_output_13_0_g12 = _NoiseMid;
				float temp_output_12_0_g12 = _NoiseStart;
				float temp_output_14_0_g12 = _NoiseEnd;
				float Noise464 = ( temp_output_10_0_g12 <= temp_output_13_0_g12 ? saturate( (0.0 + (temp_output_10_0_g12 - temp_output_12_0_g12) * (0.5 - 0.0) / (temp_output_13_0_g12 - temp_output_12_0_g12)) ) : saturate( (0.5 + (temp_output_10_0_g12 - temp_output_13_0_g12) * (1.0 - 0.5) / (temp_output_14_0_g12 - temp_output_13_0_g12)) ) );
				float mulTime513 = _TimeParameters.x * ( _VoronoiVerticalSpeed * -1.0 );
				float2 appendResult515 = (float2(RandomOffsetPixelated544 , ( floor( ( Pixel_Resolution199 * mulTime513 ) ) / Pixel_Resolution199 )));
				float2 texCoord8 = IN.ase_texcoord.xy * float2( 1,1 ) + appendResult515;
				float mulTime41 = _TimeParameters.x * _VoronoiTwirlSpeed;
				float2 uv17 = 0;
				float3 unityVoronoy17 = UnityVoronoi(( floor( ( Pixel_Resolution199 * texCoord8 ) ) / Pixel_Resolution199 ),mulTime41,_VoronoiScale,uv17);
				float temp_output_10_0_g9 = unityVoronoy17.x;
				float temp_output_13_0_g9 = _VoronoiMid;
				float temp_output_12_0_g9 = _VoronoiStart;
				float temp_output_14_0_g9 = _VoronoiEnd;
				float Voronoi50 = ( temp_output_10_0_g9 <= temp_output_13_0_g9 ? saturate( (0.0 + (temp_output_10_0_g9 - temp_output_12_0_g9) * (0.5 - 0.0) / (temp_output_13_0_g9 - temp_output_12_0_g9)) ) : saturate( (0.5 + (temp_output_10_0_g9 - temp_output_13_0_g9) * (1.0 - 0.5) / (temp_output_14_0_g9 - temp_output_13_0_g9)) ) );
				float temp_output_252_0 = ( ( _AddNoiseAmount * Noise464 ) + ( Noise464 * Voronoi50 ) + ( Voronoi50 * _AddVoronoiAmount ) );
				float temp_output_549_0 = saturate( temp_output_252_0 );
				float All_Noises244 = temp_output_549_0;
				float FinalGrayScaleMask465 = pow( saturate( ( VisibilityZone268 * ( VisibilityZone268 + All_Noises244 ) ) ) , _FinalMaskPower );
				float3 lerpResult490 = lerp( _Color1.rgb , _Color2.rgb , saturate( (0.0 + (FinalGrayScaleMask465 - _Color1Pure) * (1.0 - 0.0) / (_Color1FadeOut - _Color1Pure)) ));
				float3 lerpResult497 = lerp( _Color2.rgb , _Color3.rgb , saturate( (0.0 + (FinalGrayScaleMask465 - _Color2Pure) * (1.0 - 0.0) / (_Color2FadedOut - _Color2Pure)) ));
				float3 Color473 = ( FinalGrayScaleMask465 <= _Color1FadeOut ? lerpResult490 : lerpResult497 );
				float temp_output_10_0_g14 = FinalGrayScaleMask465;
				float temp_output_13_0_g14 = _AlphaMid;
				float temp_output_12_0_g14 = _AlphaStart;
				float temp_output_14_0_g14 = _AlphaHigh;
				float AlphaFinal572 = ( ( temp_output_10_0_g14 <= temp_output_13_0_g14 ? saturate( (0.0 + (temp_output_10_0_g14 - temp_output_12_0_g14) * (0.5 - 0.0) / (temp_output_13_0_g14 - temp_output_12_0_g14)) ) : saturate( (0.5 + (temp_output_10_0_g14 - temp_output_13_0_g14) * (1.0 - 0.5) / (temp_output_14_0_g14 - temp_output_13_0_g14)) ) ) * _MasterAlpha );
				float4 appendResult416 = (float4(( Color473 + ( (-4.0 + (Vertical_Mask373 - 0.0) * (1.0 - -4.0) / (1.0 - 0.0)) * _VerticalGradientPostProcess * Color473 ) ) , AlphaFinal572));
				float4 temp_cast_0 = (Voronoi50).xxxx;
				float4 lerpResult469 = lerp( appendResult416 , temp_cast_0 , _DEBUGShowVoronoi);
				float4 temp_cast_1 = (Noise464).xxxx;
				float4 lerpResult470 = lerp( lerpResult469 , temp_cast_1 , _DEBUGShowNoise);
				
				float4 Color = lerpResult470;
				half4 outColor = _SelectionID;
				return outColor;
			}

            ENDHLSL
        }
		
	}
	CustomEditor "ASEMaterialInspector"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=19603
Node;AmplifyShaderEditor.RangedFloatNode;241;768,1072;Inherit;False;Property;_PixelResolution;PixelResolution;0;0;Create;True;0;0;0;False;0;False;69.04025;44.9;1;100;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;559;1665.108,-519.3768;Inherit;False;Constant;_Vector0;Vector 0;44;0;Create;True;0;0;0;False;0;False;0.5,0.5;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.TextureCoordinatesNode;558;1616,-720;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FloorOpNode;242;1072,1072;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;556;1873.108,-583.3768;Inherit;True;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;199;1248,1072;Inherit;False;Pixel Resolution;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;557;2128,-592;Inherit;True;2;2;0;FLOAT2;0,0;False;1;FLOAT2;2,-2;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;273;2432,-544;Inherit;False;199;Pixel Resolution;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCPixelate;560;2720,-768;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;315;3088,-816;Inherit;False;Pixelated Vertex Positions;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;317;5072,64;Inherit;False;315;Pixelated Vertex Positions;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.BreakToComponentsNode;316;5408,64;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.TFHCRemapNode;345;5568,80;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;-1;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;372;5072,464;Inherit;False;1727.535;889.6521;;22;371;370;369;368;352;346;351;348;341;343;342;373;349;377;367;378;379;380;363;361;360;362;Vertical Mask;0,0.4805747,0.6415094,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;347;5760,80;Inherit;False;Z Axis 0to1;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;575;416,1440;Inherit;False;0;4;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;348;5104,608;Inherit;False;347;Z Axis 0to1;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;343;5168,1136;Inherit;False;Property;_EndPoint;EndPoint;4;0;Create;True;0;0;0;False;0;False;0.9124535;0.82;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;341;5088,784;Inherit;False;Property;_StartPoint;StartPoint;2;0;Create;True;0;0;0;False;0;False;0.6464933;0.065;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;342;5104,960;Inherit;False;Property;_MidPoint;MidPoint;3;0;Create;True;0;0;0;False;0;False;0.4471616;0.519;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;532;416,1344;Inherit;False;Property;_RandomSeed;RandomSeed;1;0;Create;True;0;0;0;False;0;False;7.494699;439;1;100;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;566;672,1472;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;49;341.5321,1776;Inherit;False;3126.455;680.2788;;24;510;514;515;511;509;208;8;211;210;209;50;502;17;458;457;456;43;41;40;512;513;32;539;553;Voronoi;0.3157351,0.6504626,0.9245283,1;0;0
Node;AmplifyShaderEditor.TFHCRemapNode;351;5600,1152;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;346;5552,560;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;565;848,1376;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;32;288,2144;Inherit;False;Property;_VoronoiVerticalSpeed;VoronoiVerticalSpeed;18;0;Create;True;0;0;0;False;0;False;0.5128818;0.35;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;369;5584,784;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;367;5584,896;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;349;5744,576;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;352;5808,1168;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;542;1200,1696;Inherit;False;199;Pixel Resolution;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;533;1008,1520;Inherit;False;RandomSeed;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;540;1408,1520;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;512;512,2288;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;-1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;368;6016,1072;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;370;5904,624;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;55;960,304;Inherit;False;2563.823;753.886;;18;51;11;121;229;231;13;54;214;213;212;53;52;204;462;461;460;464;546;Noise;0.02541827,0.6415094,0.4157234,1;0;0
Node;AmplifyShaderEditor.FloorOpNode;541;1568,1584;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;513;624,2160;Inherit;False;1;0;FLOAT;-0.23;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;509;752,2048;Inherit;False;199;Pixel Resolution;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;378;6208,1072;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;377;6016,1184;Inherit;False;Property;_TailRoundness;TailRoundness;7;0;Create;True;0;0;0;False;0;False;1;1.466;0.1;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;362;6080,560;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;363;5888,768;Inherit;False;Property;_FrontRoundness;FrontRoundness;6;0;Create;True;0;0;0;False;0;False;1;2.6;0.1;100;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;545;1696,1664;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;51;768,528;Inherit;False;Property;_NoiseScrollSpeed;NoiseScrollSpeed;30;0;Create;True;0;0;0;False;0;False;0.1074575;0.51;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;510;944,2304;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;379;6384,1136;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;361;6256,576;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;528;2764,-976;Inherit;False;Property;_TrailWobbleSpeed;TrailWobbleSpeed;40;0;Create;True;0;0;0;False;0;False;0;2.82;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;544;1731.222,1514.363;Inherit;False;RandomOffsetPixelated;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;52;1184,640;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;-1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FloorOpNode;511;1104,2368;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;380;6544,1152;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;360;6464,608;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;530;2960,-1072;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;204;1040,368;Inherit;False;199;Pixel Resolution;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;53;1344,640;Inherit;False;1;0;FLOAT;-0.23;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;539;1136,2192;Inherit;False;544;RandomOffsetPixelated;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;514;1264,2336;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;371;6448,784;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;526;3104,-1136;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;291;3408,-912;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.GetLocalVarNode;548;3106.212,-973.4594;Inherit;False;544;RandomOffsetPixelated;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;212;1536,544;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;515;1408,2304;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;373;6576,816;Inherit;False;Vertical Mask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;519;3168,-1472;Inherit;False;Property;_TrailWobbleScale;TrailWobbleScale;39;0;Create;True;0;0;0;False;0;False;0;5.95;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;527;3488,-1120;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FloorOpNode;213;1616,432;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;8;1584,2224;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;521;2944,-1360;Inherit;False;373;Vertical Mask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;520;3456,-1232;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;208;1584,2048;Inherit;False;199;Pixel Resolution;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;214;1776,352;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;209;1856,2000;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;546;1456,800;Inherit;False;544;RandomOffsetPixelated;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;516;3616,-1264;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;506;3168,-1568;Inherit;False;Property;_TrailWobbleStrength;TrailWobbleStrength;38;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;522;3184,-1328;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;525;3184,-1248;Inherit;False;2;0;FLOAT;1E-05;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;54;1856,656;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FloorOpNode;210;2032,2000;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;518;3728,-1424;Inherit;False;4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;40;1840,2160;Inherit;False;Property;_VoronoiTwirlSpeed;VoronoiTwirlSpeed;19;0;Create;True;0;0;0;False;0;False;1;6;-6;6;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;13;1984,416;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;231;2000,560;Inherit;False;199;Pixel Resolution;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;517;3856,-1216;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;260;2768,-304;Inherit;False;Property;_Radius;Radius;5;0;Create;True;0;0;0;False;0;False;0.1896872;0.037;0;0.3;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;41;2128,2160;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;43;2032,2256;Inherit;False;Property;_VoronoiScale;VoronoiScale;20;0;Create;True;0;0;0;False;0;False;4.600065;4.83;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;211;2208,1936;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TFHCPixelate;229;2336,400;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;121;2320,560;Inherit;False;Property;_NoiseScale;NoiseScale;29;0;Create;True;0;0;0;False;0;False;3.328468;4.19;0;20;0;1;FLOAT;0
Node;AmplifyShaderEditor.AbsOpNode;290;3824,-1072;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;310;3088,-304;Inherit;False;Radius;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;456;2576,2048;Inherit;False;Property;_VoronoiStart;VoronoiStart;21;0;Create;True;0;0;0;False;0;False;0;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;457;2576,2128;Inherit;False;Property;_VoronoiMid;VoronoiMid;22;0;Create;True;0;0;0;False;0;False;0.5;0.453;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;458;2576,2224;Inherit;False;Property;_VoronoiEnd;VoronoiEnd;23;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.VoronoiNode;17;2384,1984;Inherit;True;0;0;1;0;1;False;1;True;True;False;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;2.84;False;3;FLOAT;0;False;3;FLOAT;0;FLOAT2;1;FLOAT2;2
Node;AmplifyShaderEditor.NoiseGeneratorNode;11;2624,384;Inherit;True;Simplex2D;True;True;2;0;FLOAT2;0,0;False;1;FLOAT;5.64;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;462;2768,752;Inherit;False;Property;_NoiseEnd;NoiseEnd;28;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;461;2768,640;Inherit;False;Property;_NoiseMid;NoiseMid;27;0;Create;True;0;0;0;False;0;False;0.5;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;460;2848,528;Inherit;False;Property;_NoiseStart;NoiseStart;26;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;502;2896,1984;Inherit;True;ValueRemap;-1;;9;826690353913e4a4b98ad0866c4a6300;0;4;10;FLOAT;0;False;12;FLOAT;0;False;13;FLOAT;0.5;False;14;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;292;4016,-1200;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;322;3984,-976;Inherit;False;310;Radius;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;50;3136,1856;Inherit;False;Voronoi;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;505;3184,416;Inherit;False;ValueRemap;-1;;12;826690353913e4a4b98ad0866c4a6300;0;4;10;FLOAT;0;False;12;FLOAT;0;False;13;FLOAT;0.5;False;14;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;302;4208,-1216;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;430;3728,784;Inherit;False;Property;_AddNoiseAmount;AddNoiseAmount;31;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;464;3312,624;Inherit;False;Noise;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;459;3552,1216;Inherit;False;50;Voronoi;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;433;3664,1696;Inherit;False;Property;_AddVoronoiAmount;AddVoronoiAmount;24;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;307;4576,-1216;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;381;4768,-1872;Inherit;False;1193.203;571.0037;;7;268;437;436;438;356;375;353;VisibilityZone;0.2826625,0.04670699,0.3018868,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;432;4016,944;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;434;4000,1408;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;431;3904,1152;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;323;4912,-1216;Inherit;False;Z Proximity;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;252;4224,1120;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;353;4784,-1824;Inherit;False;323;Z Proximity;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;375;4784,-1712;Inherit;False;373;Vertical Mask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;549;4400,1136;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;356;5056,-1824;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;438;5056,-1696;Inherit;False;Property;_VisZoneDark;VisZoneDark;8;0;Create;True;0;0;0;False;0;False;0;0;0;0.3;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;436;5056,-1568;Inherit;False;Property;_VisZoneMid;VisZoneMid;9;0;Create;True;0;0;0;False;0;False;0.3;0.009;0;0.3;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;437;5056,-1424;Inherit;False;Property;_VisZoneHigh;VisZoneHigh;10;0;Create;True;0;0;0;False;0;False;0.3;0.262;0;0.3;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;244;4640,1104;Inherit;False;All Noises;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;503;5472,-1776;Inherit;False;ValueRemap;-1;;13;826690353913e4a4b98ad0866c4a6300;0;4;10;FLOAT;0;False;12;FLOAT;0;False;13;FLOAT;0.5;False;14;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;268;5696,-1776;Inherit;False;VisibilityZone;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;382;6016,-1440;Inherit;False;244;All Noises;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;410;6176,-1616;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;383;6336,-1776;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;394;6576,-1680;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;555;6528,-1488;Inherit;False;Property;_FinalMaskPower;FinalMaskPower;43;0;Create;True;0;0;0;False;0;False;1;0.54;0;3;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;554;6736,-1664;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;465;6896,-1696;Inherit;False;FinalGrayScaleMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;485;7664,624;Inherit;False;Property;_Color1FadeOut;Color 1 Fade Out;13;0;Create;True;0;0;0;False;0;False;0.5;0.059;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;484;7648,304;Inherit;False;Property;_Color1Pure;Color 1 Pure;12;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;486;8160,1008;Inherit;False;Property;_Color2Pure;Color 2 Pure;15;0;Create;True;0;0;0;False;0;False;1;0.296;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;487;8160,1088;Inherit;False;Property;_Color2FadedOut;Color 2 Faded Out;16;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;494;8320,352;Inherit;False;465;FinalGrayScaleMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;488;7904,336;Inherit;False;465;FinalGrayScaleMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;491;8048,736;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;495;8464,800;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;467;7392,-576;Inherit;False;1190.404;584.2403;;8;572;571;570;453;454;452;504;466;Alpha;0.1631007,0.4622642,0.4301368,1;0;0
Node;AmplifyShaderEditor.ColorNode;251;7712,416;Inherit;False;Property;_Color1;Color 1;11;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,1;0,0.1509434,0,1;True;True;0;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.SaturateNode;492;8256,864;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;496;8704,848;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;85;8576,1056;Inherit;False;Property;_Color3;Color 3;17;1;[HDR];Create;True;0;0;0;False;0;False;0.4488788,0,0,0;0.4634897,1.844303,0,1;True;True;0;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.ColorNode;6;7712,880;Inherit;False;Property;_Color2;Color 2;14;1;[HDR];Create;True;0;0;0;False;0;False;8.909804,0.7529412,0,0;0.004338408,0.1886792,0,1;True;True;0;6;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT3;5
Node;AmplifyShaderEditor.LerpOp;490;8208,464;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;497;8867.883,945.7117;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;466;7440,-528;Inherit;False;465;FinalGrayScaleMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;452;7440,-448;Inherit;False;Property;_AlphaStart;AlphaStart;34;0;Create;True;0;0;0;False;0;False;0;0.367;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;454;7440,-368;Inherit;False;Property;_AlphaMid;AlphaMid;35;0;Create;True;0;0;0;False;0;False;0.5;0.407;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;453;7440,-288;Inherit;False;Property;_AlphaHigh;AlphaHigh;36;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Compare;493;8944,624;Inherit;False;5;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;475;7920,-1184;Inherit;False;373;Vertical Mask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;504;7792,-464;Inherit;False;ValueRemap;-1;;14;826690353913e4a4b98ad0866c4a6300;0;4;10;FLOAT;0;False;12;FLOAT;0;False;13;FLOAT;0.5;False;14;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;570;7696,-192;Inherit;False;Property;_MasterAlpha;MasterAlpha;33;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;473;8816,256;Inherit;False;Color;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;472;8208,-1040;Inherit;False;Property;_VerticalGradientPostProcess;VerticalGradientPostProcess;37;0;Create;True;0;0;0;False;0;False;0;0.193;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;478;8208,-1216;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;-4;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;571;8032,-368;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;482;8640.303,-1057.563;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;572;8208,-368;Inherit;False;AlphaFinal;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;483;8832,-1008;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;573;8928,-608;Inherit;False;572;AlphaFinal;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;416;9104,-800;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;421;8832,-1728;Inherit;False;50;Voronoi;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;450;8768,-1632;Inherit;False;Property;_DEBUGShowVoronoi;DEBUGShowVoronoi;25;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;469;9200,-1680;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;471;9312,-1424;Inherit;False;464;Noise;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;468;9216,-1312;Inherit;False;Property;_DEBUGShowNoise;DEBUGShowNoise;32;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;259;1808,-944;Inherit;True;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;261;2064,-944;Inherit;True;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;266;2352,-944;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;5,5;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;553;2208,2352;Inherit;False;Property;_VoronoiSmoothness;VoronoiSmoothness;42;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;552;4320,1488;Inherit;False;Property;_Float0;Float 0;41;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;551;4528,1264;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;550;4345.964,1284.196;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StickyNoteNode;306;2624,-1104;Inherit;False;294.3347;125.4708;Why 5;;1,1,1,1;Using 5 because that is the length of Unity's default plane, so we can work more easily with a unit vector;0;0
Node;AmplifyShaderEditor.LerpOp;470;9552,-1648;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;569;9328,-864;Inherit;False;0;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StickyNoteNode;576;192,1008;Inherit;False;310.9592;144.6357;;;1,1,1,1;This one is used when adding a custom vertex stream to a particle system --> random --> stableX. It's predefined by Unity that this random value between 0 and 1 will be passed through the VertexTexCoord.Z;0;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;0,0;Float;False;False;-1;2;ASEMaterialInspector;0;15;New Amplify Shader;cf964e524c8e69742b1d21fbe2ebcc4a;True;Sprite Unlit Forward;0;1;Sprite Unlit Forward;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;0;True;12;all;0;False;True;2;5;False;;10;False;;3;1;False;;10;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;2;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;ASEMaterialInspector;0;15;New Amplify Shader;cf964e524c8e69742b1d21fbe2ebcc4a;True;SceneSelectionPass;0;2;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;0;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;ASEMaterialInspector;0;15;New Amplify Shader;cf964e524c8e69742b1d21fbe2ebcc4a;True;ScenePickingPass;0;3;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;0;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;9728,-1248;Float;False;True;-1;2;ASEMaterialInspector;0;15;Fire;cf964e524c8e69742b1d21fbe2ebcc4a;True;Sprite Unlit;0;0;Sprite Unlit;4;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;0;True;12;all;0;False;True;2;5;False;;10;False;;3;1;False;;10;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;2;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;Hidden/InternalErrorShader;0;0;Standard;3;Vertex Position;1;0;Debug Display;0;0;External Alpha;0;0;0;4;True;True;True;True;False;;False;0
WireConnection;242;0;241;0
WireConnection;556;0;558;0
WireConnection;556;1;559;0
WireConnection;199;0;242;0
WireConnection;557;0;556;0
WireConnection;560;0;557;0
WireConnection;560;1;273;0
WireConnection;560;2;273;0
WireConnection;315;0;560;0
WireConnection;316;0;317;0
WireConnection;345;0;316;1
WireConnection;347;0;345;0
WireConnection;566;0;575;3
WireConnection;351;0;348;0
WireConnection;351;1;342;0
WireConnection;351;2;343;0
WireConnection;346;0;348;0
WireConnection;346;1;341;0
WireConnection;346;2;342;0
WireConnection;565;0;532;0
WireConnection;565;1;566;0
WireConnection;369;0;348;0
WireConnection;369;1;342;0
WireConnection;367;0;342;0
WireConnection;367;1;348;0
WireConnection;349;0;346;0
WireConnection;352;0;351;0
WireConnection;533;0;565;0
WireConnection;540;0;533;0
WireConnection;540;1;542;0
WireConnection;512;0;32;0
WireConnection;368;0;352;0
WireConnection;368;1;367;0
WireConnection;370;0;349;0
WireConnection;370;1;369;0
WireConnection;541;0;540;0
WireConnection;513;0;512;0
WireConnection;378;0;368;0
WireConnection;362;0;370;0
WireConnection;545;0;541;0
WireConnection;545;1;542;0
WireConnection;510;0;509;0
WireConnection;510;1;513;0
WireConnection;379;0;378;0
WireConnection;379;1;377;0
WireConnection;361;0;362;0
WireConnection;361;1;363;0
WireConnection;544;0;545;0
WireConnection;52;0;51;0
WireConnection;511;0;510;0
WireConnection;380;0;379;0
WireConnection;360;0;361;0
WireConnection;530;0;528;0
WireConnection;53;0;52;0
WireConnection;514;0;511;0
WireConnection;514;1;509;0
WireConnection;371;0;360;0
WireConnection;371;1;380;0
WireConnection;526;0;530;0
WireConnection;291;0;315;0
WireConnection;212;0;204;0
WireConnection;212;1;53;0
WireConnection;515;0;539;0
WireConnection;515;1;514;0
WireConnection;373;0;371;0
WireConnection;527;0;291;1
WireConnection;527;1;526;0
WireConnection;527;2;548;0
WireConnection;213;0;212;0
WireConnection;8;1;515;0
WireConnection;520;0;519;0
WireConnection;520;1;527;0
WireConnection;214;0;213;0
WireConnection;214;1;204;0
WireConnection;209;0;208;0
WireConnection;209;1;8;0
WireConnection;516;0;520;0
WireConnection;522;0;521;0
WireConnection;525;1;521;0
WireConnection;54;0;546;0
WireConnection;54;1;214;0
WireConnection;210;0;209;0
WireConnection;518;0;506;0
WireConnection;518;1;522;0
WireConnection;518;2;525;0
WireConnection;518;3;516;0
WireConnection;13;1;54;0
WireConnection;517;0;518;0
WireConnection;517;1;291;0
WireConnection;41;0;40;0
WireConnection;211;0;210;0
WireConnection;211;1;208;0
WireConnection;229;0;13;0
WireConnection;229;1;231;0
WireConnection;229;2;231;0
WireConnection;290;0;517;0
WireConnection;310;0;260;0
WireConnection;17;0;211;0
WireConnection;17;1;41;0
WireConnection;17;2;43;0
WireConnection;11;0;229;0
WireConnection;11;1;121;0
WireConnection;502;10;17;0
WireConnection;502;12;456;0
WireConnection;502;13;457;0
WireConnection;502;14;458;0
WireConnection;292;0;290;0
WireConnection;50;0;502;0
WireConnection;505;10;11;0
WireConnection;505;12;460;0
WireConnection;505;13;461;0
WireConnection;505;14;462;0
WireConnection;302;0;292;0
WireConnection;302;4;322;0
WireConnection;464;0;505;0
WireConnection;307;0;302;0
WireConnection;432;0;430;0
WireConnection;432;1;464;0
WireConnection;434;0;50;0
WireConnection;434;1;433;0
WireConnection;431;0;464;0
WireConnection;431;1;459;0
WireConnection;323;0;307;0
WireConnection;252;0;432;0
WireConnection;252;1;431;0
WireConnection;252;2;434;0
WireConnection;549;0;252;0
WireConnection;356;0;353;0
WireConnection;356;1;375;0
WireConnection;244;0;549;0
WireConnection;503;10;356;0
WireConnection;503;12;438;0
WireConnection;503;13;436;0
WireConnection;503;14;437;0
WireConnection;268;0;503;0
WireConnection;410;0;268;0
WireConnection;410;1;382;0
WireConnection;383;0;268;0
WireConnection;383;1;410;0
WireConnection;394;0;383;0
WireConnection;554;0;394;0
WireConnection;554;1;555;0
WireConnection;465;0;554;0
WireConnection;491;0;488;0
WireConnection;491;1;484;0
WireConnection;491;2;485;0
WireConnection;495;0;494;0
WireConnection;495;1;486;0
WireConnection;495;2;487;0
WireConnection;492;0;491;0
WireConnection;496;0;495;0
WireConnection;490;0;251;5
WireConnection;490;1;6;5
WireConnection;490;2;492;0
WireConnection;497;0;6;5
WireConnection;497;1;85;5
WireConnection;497;2;496;0
WireConnection;493;0;494;0
WireConnection;493;1;485;0
WireConnection;493;2;490;0
WireConnection;493;3;497;0
WireConnection;504;10;466;0
WireConnection;504;12;452;0
WireConnection;504;13;454;0
WireConnection;504;14;453;0
WireConnection;473;0;493;0
WireConnection;478;0;475;0
WireConnection;571;0;504;0
WireConnection;571;1;570;0
WireConnection;482;0;478;0
WireConnection;482;1;472;0
WireConnection;482;2;473;0
WireConnection;572;0;571;0
WireConnection;483;0;473;0
WireConnection;483;1;482;0
WireConnection;416;0;483;0
WireConnection;416;3;573;0
WireConnection;469;0;416;0
WireConnection;469;1;421;0
WireConnection;469;2;450;0
WireConnection;261;0;259;1
WireConnection;261;1;259;3
WireConnection;266;0;261;0
WireConnection;551;0;549;0
WireConnection;551;1;552;0
WireConnection;550;0;252;0
WireConnection;470;0;469;0
WireConnection;470;1;471;0
WireConnection;470;2;468;0
WireConnection;0;1;470;0
ASEEND*/
//CHKSM=7E16640CFF80D4EB12CB979AAE5E3243EA59E933