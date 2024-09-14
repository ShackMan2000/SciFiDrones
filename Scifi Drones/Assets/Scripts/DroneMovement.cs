using System;
using System.Collections.Generic;
using Sirenix.OdinInspector;
using Unity.VisualScripting;
using UnityEngine;


public class DroneMovement : MonoBehaviour
{
    [SerializeField] float maxForwardSpeed;
    [SerializeField] float maxForwardPitch;
    
    [SerializeField] float maxSidewaysSpeed;
    [SerializeField] float maxSidewaysRoll;
    
    [SerializeField] float maxYawSpeed;
    
    [SerializeField] float timeToReachTarget = 1f;
    //  [SerializeField] AnimationCurve reachTargetCurve;


    [SerializeField] Transform gravityCenter;

    public float forwardMovement;
    public float sidewaysMovement;


    float forwardTarget;
    float sidewaysTarget;
    float yawTarget;


    public event Action<float> OnForwardTargetChanged = delegate { };
    public event Action<float> OnSidewaysTargetChanged = delegate { };
    public event Action<float> OnYawTargetChanged = delegate { };

    
    [Button]
    public void SetForwardTarget(float newTarget)
    {
        forwardTarget = newTarget;
        OnForwardTargetChanged?.Invoke(newTarget);
    }

    
    [Button]
    public void SetSidewaysTarget(float sideInput)
    {
        sidewaysTarget = sideInput;
        OnSidewaysTargetChanged?.Invoke(sideInput);
    }


    public void SetYawTarget(float yawInput)
    {
        yawTarget = yawInput;
        OnYawTargetChanged?.Invoke(yawInput);
    }
    
    
    
    void Update()
    {
        forwardMovement = Mathf.Lerp(forwardMovement, forwardTarget, Time.deltaTime / timeToReachTarget);
        sidewaysMovement = Mathf.Lerp(sidewaysMovement, sidewaysTarget, Time.deltaTime / timeToReachTarget);
        
        
        //transform.position += transform.forward * currentValue * maxForwardSpeed * Time.deltaTime;
        
        float yRotation = gravityCenter.localRotation.eulerAngles.y + yawTarget * maxYawSpeed * Time.deltaTime;
        //yRotation +=
        
       gravityCenter.localRotation = Quaternion.Euler(forwardMovement * maxForwardPitch, yRotation , sidewaysMovement * maxSidewaysRoll);
        
        //gravityCenter.Rotate(Vector3.up, yawTarget * maxYawSpeed * Time.deltaTime, Space.World  );
        // for forward we can just rotate the thrusters directly, no real need for a difference, polish later
    }


// [ContextMenu("toggle the unknown")]
//     void yhyhjhjmh()
//     {
//         if (Input.GetKeyDown(KeyCode.Space))
//         {
//             shitIsOn = !shitIsOn;
//             OVRManager.eyeFovPremultipliedAlphaModeEnabled = shitIsOn;
//         }
//     }
}