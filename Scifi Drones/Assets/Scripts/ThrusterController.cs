using System;
using System.Collections.Generic;
using System.Linq;
using Sirenix.OdinInspector;
using UnityEngine;

public class ThrusterController : MonoBehaviour
{
    List<Thruster> allThrusters => new List<Thruster>(leftThrusters.Concat(rightThrusters));
    public List<Thruster> leftThrusters;
    public List<Thruster> rightThrusters;
  
    [SerializeField] DroneMovement droneMovement;

    float forwardTarget;
    float sidewaysTarget;

    float currentForward;
    float currentSideways;
    
    public float forwardMaxRotation;
    public float sideMaxRotation;
    [SerializeField] float timeToReachTarget = 0.3f;


    void OnEnable()
    {
        droneMovement.OnForwardTargetChanged += SetForwardTarget;
        droneMovement.OnSidewaysTargetChanged += SetSideWaysTarget;
    }

    void OnDisable()
    {
        droneMovement.OnForwardTargetChanged -= SetForwardTarget;
        droneMovement.OnSidewaysTargetChanged -= SetSideWaysTarget;
    }
 

    void SetForwardTarget(float forward)
    {
        forwardTarget = forward;
    }
    
    
    void SetSideWaysTarget(float sidewaysMovement)
    {
        MoveSideways(sidewaysMovement);
    }
    
    
    
    // by starting to lerp from the previous value, we move faster the further away we are from the target
    // unlike with the drone movement, we don't have to worry about getting rid of really small values in the end
    void Update()
    {
        currentForward = Mathf.Lerp(currentForward, forwardTarget, Time.deltaTime / timeToReachTarget);
        SetRingRotationsForForward(currentForward);
        
        currentSideways = Mathf.Lerp(currentSideways, sidewaysTarget, Time.deltaTime / timeToReachTarget);
        MoveSideways(currentSideways);
    }
    

    [Button]
    void SetRingRotationsForForward(float adjustedForward)
    {
        foreach (var thruster in allThrusters)
        {
            if (thruster.IsLockedForYaw) continue;

            float rotation = forwardMaxRotation * adjustedForward;
            //thruster.Ring.rotation = new Quaternion(0,0,0,0);
            thruster.Ring.localRotation = Quaternion.Euler(rotation, 0, 0);
        }
    }





  




 

    void MoveSideways(float sidewaysMovement)
    {
        if (sidewaysMovement >= 0)
        {
            foreach (var thruster in leftThrusters)
            {
                if (thruster.IsLockedForYaw) continue;

                float rotation = sideMaxRotation * sidewaysMovement;
                thruster.Flapper.localRotation = Quaternion.Euler(0, 0, rotation);
            }

            foreach (var thruster in rightThrusters)
            {
                if (thruster.IsLockedForYaw) continue;

                float rotation = -sideMaxRotation * sidewaysMovement;
                thruster.Flapper.localRotation = Quaternion.Euler(0, 0, rotation);
            }
        }
        else if (sidewaysMovement < 0)
        {
            foreach (var thruster in leftThrusters)
            {
                if (thruster.IsLockedForYaw) continue;
                float rotation = sideMaxRotation * sidewaysMovement;
                thruster.Flapper.localRotation = Quaternion.Euler(0, 0, rotation);
            }

            foreach (var thruster in rightThrusters)
            {
                if (thruster.IsLockedForYaw) continue;
                float rotation = -sideMaxRotation * sidewaysMovement;
                thruster.Flapper.localRotation = Quaternion.Euler(0, 0, rotation);
            }
        }
    }
}




// [Range(0, 1)]
// public float forwardCapWhenLateralMovement;

// [SerializeField] Thruster thrusterYawClockwiseFront;
// [SerializeField] Thruster thrusterYawClockwiseBack;
//
// [SerializeField] Thruster thrusterYawCounterClockwiseFront;
// [SerializeField] Thruster thrusterYawCounterClockwiseBack;
//
// [SerializeField] float yawMaxFlapRotation;
// [SerializeField] float yawRingRotation;

//
// [Button]
// void SetRotationForYaw(float yaw)
// {
//     foreach (var thruster in allThrusters)
//     {
//         thruster.IsLockedForYaw = false;
//     }
//
//     // instead of just locking it, might want to lerp it, the stronger the yaw the more we override the other thrusters
//
//     float flapRotation = yawMaxFlapRotation * yaw;
//     float ringRotation = yawRingRotation * yaw;
//
//     if (yaw > 0.05f)
//     {
//         thrusterYawClockwiseFront.Flapper.localRotation = Quaternion.Euler(0, 0, flapRotation);
//         thrusterYawClockwiseFront.Ring.localRotation = Quaternion.Euler(ringRotation, 0, 0);
//
//         thrusterYawClockwiseBack.Flapper.localRotation = Quaternion.Euler(0, 0, flapRotation);
//         thrusterYawClockwiseBack.Ring.localRotation = Quaternion.Euler(-ringRotation, 0, 0);
//
//         thrusterYawClockwiseFront.IsLockedForYaw = true;
//         thrusterYawClockwiseBack.IsLockedForYaw = true;
//     }
//     else if (yaw < -0.05)
//     {
//         flapRotation = -flapRotation;
//
//         thrusterYawCounterClockwiseFront.Flapper.localRotation = Quaternion.Euler(0, 0, flapRotation);
//         thrusterYawCounterClockwiseFront.Ring.localRotation = Quaternion.Euler(-ringRotation, 0, 0);
//
//         thrusterYawCounterClockwiseBack.Flapper.localRotation = Quaternion.Euler(0, 0, flapRotation);
//         thrusterYawCounterClockwiseBack.Ring.localRotation = Quaternion.Euler(ringRotation, 0, 0);
//
//         thrusterYawCounterClockwiseFront.IsLockedForYaw = true;
//         thrusterYawCounterClockwiseBack.IsLockedForYaw = true;
//     }
// }