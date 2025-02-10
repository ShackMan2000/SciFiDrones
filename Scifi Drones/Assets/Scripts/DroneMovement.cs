using System;
using System.Collections.Generic;
using Sirenix.OdinInspector;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.Serialization;


public class DroneMovement : MonoBehaviour
{
    
    [BoxGroup("Movement")] [SerializeField] float maxForwardSpeed;
    [BoxGroup("Movement")] [SerializeField] float maxSidewaysSpeed;
    [BoxGroup("Movement")] [SerializeField] float maxYawSpeed;
    [BoxGroup("Movement")] [SerializeField] float maxVerticalSpeed;

    [SerializeField] float maxSidewaysRoll;
    [SerializeField] float maxForwardPitch;
    [SerializeField] float timeToReachTarget = 1f;
    //  [SerializeField] AnimationCurve reachTargetCurve;


    [SerializeField] Transform pitchAndRollCenter;
    [SerializeField] Transform yawCenter;

    public float pitchChange;

    float pitchTarget;
    float sidewaysTarget;
    float yawTarget;
    float verticalTarget;


    public event Action<float> OnForwardTargetChanged = delegate { };
    public event Action<float> OnSidewaysTargetChanged = delegate { };
    public event Action<float> OnYawTargetChanged = delegate { };


    [Button]
    public void SetForwardTarget(float newTarget)
    {
        pitchTarget = newTarget * maxForwardPitch;
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


    public void SetVerticalTarget(float verticalInput)
    {
        verticalTarget = verticalInput;
    }
    

    float currentPitch;
    float currentRoll;

    
    void Update()
    {

        currentPitch = Mathf.Lerp(currentPitch, pitchTarget, Time.deltaTime / timeToReachTarget);

        currentPitch += pitchChange;

        currentRoll = Mathf.Lerp(currentRoll, sidewaysTarget * maxSidewaysRoll, Time.deltaTime / timeToReachTarget);
        pitchAndRollCenter.transform.localRotation = Quaternion.Euler(new Vector3(currentPitch, 0, currentRoll));


        Move();
    }

    float currentForwardSpeed;
    float currentSidewaysSpeed;
    float currentYawSpeed;
    

    
    
    [SerializeField] float timeToReachSpeedTarget = 1f;
    
    // don't get stuck here, doesn't matter! Just do something and try it out
    void Move()
    {
        
        float forwardSpeedTarget = currentPitch * maxForwardSpeed;
        
        currentForwardSpeed = Mathf.Lerp(currentForwardSpeed, forwardSpeedTarget, Time.deltaTime / timeToReachSpeedTarget);
        
        
        float sidewaysSpeedTarget = currentRoll * maxSidewaysSpeed;
         currentSidewaysSpeed = Mathf.Lerp(currentSidewaysSpeed, sidewaysSpeedTarget, Time.deltaTime / timeToReachSpeedTarget);
        
         
         
         Vector3 movement = yawCenter.forward * currentForwardSpeed + pitchAndRollCenter.right * currentSidewaysSpeed;
         yawCenter.position += movement * Time.deltaTime;
         
         float yawSpeedTarget = yawTarget * maxYawSpeed;
         currentYawSpeed = Mathf.Lerp(currentYawSpeed, yawSpeedTarget, Time.deltaTime / timeToReachSpeedTarget);
         
        yawCenter.transform.Rotate(0, currentYawSpeed * Time.deltaTime, 0, Space.Self);
    }

}