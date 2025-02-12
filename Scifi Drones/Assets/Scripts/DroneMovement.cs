﻿using System;
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


    [SerializeField] Transform pitchAndRollCenter;
    [SerializeField] Transform yawCenter;
    [SerializeField] float timeToReachSpeedTarget = 1f;
    [SerializeField] float forcedInertiaInSeconds;


    float pitchTarget;
    float sidewaysTarget;
    float yawTarget;
    float verticalTarget;


    public event Action<float> OnForwardTargetChanged = delegate { };
    public event Action<float> OnSidewaysTargetChanged = delegate { };
    public event Action<float> OnYawTargetChanged = delegate { };


    float currentForwardSpeed;
    float currentSidewaysSpeed;
    float currentYawSpeed;
    float currentVerticalSpeed;

    float currentPitch;
    float currentRoll;


    public bool LockMovement;

   
    
    
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



    void Update()
    {
        // the problem here is that the closer we get to the target, the less we move towards it
        // which is fine for acceleration
        // but if the target is 0, we shouldn't lerp from e.g. 0.02 to 0
        // and in th next frame 0.01 to 0
        // we could also do an extra method that if the target is 0, we reduce the speed by some linear amount


        currentPitch = Mathf.Lerp(currentPitch, pitchTarget, Time.deltaTime / timeToReachTarget);

        if (Mathf.Abs(pitchTarget) < 0.01f)
        {
            float forcedInertiaPerSecond = maxForwardPitch / forcedInertiaInSeconds;
            float forcedInertiaThisFrame = forcedInertiaPerSecond * Time.deltaTime;

            if (currentPitch > 0)
            {
                currentPitch = Mathf.Max(0, currentPitch - forcedInertiaThisFrame);
            }
            else if (currentPitch < 0)
            {
                currentPitch = Mathf.Min(0, currentPitch + forcedInertiaThisFrame);
            }
        }

        currentRoll = Mathf.Lerp(currentRoll, sidewaysTarget * maxSidewaysRoll, Time.deltaTime / timeToReachTarget);

        if (Mathf.Abs(sidewaysTarget) < 0.01f)
        {
            float forcedInertiaPerSecond = maxSidewaysRoll / forcedInertiaInSeconds;
            float forcedInertiaThisFrame = forcedInertiaPerSecond * Time.deltaTime;

            if (currentRoll > 0)
            {
                currentRoll = Mathf.Max(0, currentRoll - forcedInertiaThisFrame);
            }
            else if (currentRoll < 0)
            {
                currentRoll = Mathf.Min(0, currentRoll + forcedInertiaThisFrame);
            }
        }


        pitchAndRollCenter.transform.localRotation = Quaternion.Euler(new Vector3(currentPitch, 0, currentRoll * -1f));

        Move();
    }


    void Move()
    {
        if (LockMovement)
        {
            return;
        }

        float forwardSpeedTarget = currentPitch * maxForwardSpeed;

        currentForwardSpeed = Mathf.Lerp(currentForwardSpeed, forwardSpeedTarget, Time.deltaTime / timeToReachSpeedTarget);


        float sidewaysSpeedTarget = currentRoll * maxSidewaysSpeed;
        currentSidewaysSpeed = Mathf.Lerp(currentSidewaysSpeed, sidewaysSpeedTarget, Time.deltaTime / timeToReachSpeedTarget);


        Vector3 movement = yawCenter.forward * currentForwardSpeed + pitchAndRollCenter.right * currentSidewaysSpeed;
        yawCenter.position += movement * Time.deltaTime;

        float yawSpeedTarget = yawTarget * maxYawSpeed;
        currentYawSpeed = Mathf.Lerp(currentYawSpeed, yawSpeedTarget, Time.deltaTime / timeToReachSpeedTarget);

        
        if(Mathf.Abs(yawTarget) < 0.01f)
        {
            float forcedInertiaPerSecond = maxYawSpeed / forcedInertiaInSeconds;
            float forcedInertiaThisFrame = forcedInertiaPerSecond * Time.deltaTime;

            if (currentYawSpeed > 0)
            {
                currentYawSpeed = Mathf.Max(0, currentYawSpeed - forcedInertiaThisFrame);
            }
            else if (currentYawSpeed < 0)
            {
                currentYawSpeed = Mathf.Min(0, currentYawSpeed + forcedInertiaThisFrame);
            }
        }
        
        yawCenter.transform.Rotate(0, currentYawSpeed * Time.deltaTime, 0, Space.Self);

        float verticalSpeedTarget = verticalTarget * maxVerticalSpeed;
        currentVerticalSpeed = Mathf.Lerp(currentVerticalSpeed, verticalSpeedTarget, Time.deltaTime / timeToReachSpeedTarget);

        yawCenter.position += pitchAndRollCenter.up * currentVerticalSpeed * Time.deltaTime;
    }

}