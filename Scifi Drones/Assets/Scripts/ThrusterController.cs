using System;
using System.Collections.Generic;
using System.Linq;
using Sirenix.OdinInspector;
using UnityEngine;

public class ThrusterController : MonoBehaviour
{
    // 2 guiding principles
    // It should look cool and feel responsive -> thrusters should move immediatly and maybe some extra feedback for sharp value changes
    // even if that doesn't make sense, e.g. change the side movement drastically could just add some extra sparks
    // Take every short cut you can

    public List<Thruster> allThrusters => new List<Thruster>(leftThrusters.Concat(rightThrusters));
    public List<Thruster> leftThrusters;
    public List<Thruster> rightThrusters;


    [SerializeField] Thruster thrusterYawClockwiseFront;
    [SerializeField] Thruster thrusterYawClockwiseBack;

    [SerializeField] Thruster thrusterYawCounterClockwiseFront;
    [SerializeField] Thruster thrusterYawCounterClockwiseBack;

    [SerializeField] float yawMaxFlapRotation;
    [SerializeField] float yawRingRotation;

    // going through the drone movement instead of getting the input right away
    // in case the thrusters should be adjusted based on input in relation to current movement
    [SerializeField] DroneMovement droneMovement;

    // input controller
    // take the current input and use that as a target
    // one curve for increasing speed
    // maybe a second one for decreasing, but do that later

    // that will be the value that determines the pitch too, so it's delayed
    // while the thrusters react immediatly to the input and use the difference to the drone movement
    // e.g. if there is side movement, the thrusters flap to the side based
    // on how much the drone is not rolled....


    void OnEnable()
    {
        droneMovement.OnForwardTargetChanged += SetForwardMovement;
        droneMovement.OnSidewaysTargetChanged += SetSideMovement;
        droneMovement.OnYawTargetChanged += SetRotationForYaw;
    }

    void OnDisable()
    {
        droneMovement.OnForwardTargetChanged -= SetForwardMovement;
        droneMovement.OnSidewaysTargetChanged -= SetSideMovement;
        droneMovement.OnYawTargetChanged -= SetRotationForYaw;
    }


    // use this extra method since I might want to add some VFX stuff
    void SetForwardMovement(float forward)
    {
        SetRingRotationsForForward(forward);
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

    [Button]
    void SetRotationForYaw(float yaw)
    {
        foreach (var thruster in allThrusters)
        {
            thruster.IsLockedForYaw = false;
        }

        // instead of just locking it, might want to lerp it, the stronger the yaw the more we override the other thrusters

        float flapRotation = yawMaxFlapRotation * yaw;
        float ringRotation = yawRingRotation * yaw;

        if (yaw > 0.05f)
        {
            thrusterYawClockwiseFront.Flapper.localRotation = Quaternion.Euler(0, 0, flapRotation);
            thrusterYawClockwiseFront.Ring.localRotation = Quaternion.Euler(ringRotation, 0, 0);

            thrusterYawClockwiseBack.Flapper.localRotation = Quaternion.Euler(0, 0, flapRotation);
            thrusterYawClockwiseBack.Ring.localRotation = Quaternion.Euler(-ringRotation, 0, 0);

            thrusterYawClockwiseFront.IsLockedForYaw = true;
            thrusterYawClockwiseBack.IsLockedForYaw = true;
        }
        else if (yaw < -0.05)
        {
            flapRotation = -flapRotation;

            thrusterYawCounterClockwiseFront.Flapper.localRotation = Quaternion.Euler(0, 0, flapRotation);
            thrusterYawCounterClockwiseFront.Ring.localRotation = Quaternion.Euler(-ringRotation, 0, 0);

            thrusterYawCounterClockwiseBack.Flapper.localRotation = Quaternion.Euler(0, 0, flapRotation);
            thrusterYawCounterClockwiseBack.Ring.localRotation = Quaternion.Euler(ringRotation, 0, 0);

            thrusterYawCounterClockwiseFront.IsLockedForYaw = true;
            thrusterYawCounterClockwiseBack.IsLockedForYaw = true;
        }
    }


    // for forward movement it might look more interesting if the drone pitches
    // maybe this could be speed dependent and a step towards the high speed mode:
    // at slow speeds the drone pitches forward, so it's more like a curve with a small bump
    // and then when reaching higher and higher speeds it actually becomes less
    // so the thrusters are also capped in their forward rotation until reaching those higher speeds.
    // and maybe for backwards just cap it at max pitch. Like just set a number e.g. 0.3 of the forward curve
    // and that's it.


    // e.g. the input says move to the side but the drone is not tilted like that:
    // so the thrusters move immediatly
    // 


    public float forwardMaxRotation;
    public float sideMaxRotation;


    // use a hierarchy: Rotating around the own axis reserves front and crossed to the back
    // side to side can steal a bit of the forward rotation

    [Range(0, 1)]
    public float forwardCapWhenLateralMovement;


    [Button]
    void SetSideMovement(float sidewaysMovement)
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


    // every thruster will revert to a default position (use a curve)


    // forward movement:
    // the ring needs to be rotated 90 degrees, flapper can be anything (maybe later move down for highspeed)

    // side to side:
    // the flappers on one side has to move down, up on the other
    // the rings can not be rotated 90 degrees here. maybe just remove a bit of that
    // e.g. reserve a bit for max side movement.
    // entire drone should rotate though, looks way better. So maybe keep the thrusters subtle

    // rotate around the own axis:
    // reserve a front and back on the otherside for each direction.
    // This has priority over all the others


    // up and down:
    // if the drone isn't moving forward, could just happen by adding more thrust or reversing it
    // so the only is when the thrusters are in forward rotation
    // in that case it would be cool if the drone tilts (maybe the tilt is based on the current forward)
    // the thrusters then should all rotate a bit
    // not sure yet how to handle when side to side is happening also
    // could just try it out, make some sliders etc.
}


// for side to side maybe a delay where the thrusters flap a bit more, call it overflap.
// void OnValidate()
// {
//     return;
//
//     float adjustedForward = forwardRelative;
//     // if the rings are fully facing forward, we need to limit it a bit so there is side movement
//     // if (Mathf.Abs(sidewaysMovement) > 0 && Mathf.Abs(forwardRelative) > forwardCapWhenLateralMovement)
//     // {
//     //     if (forwardRelative > 0)
//     //     {
//     //         adjustedForward = forwardCapWhenLateralMovement;
//     //     }
//     //     else
//     //     {
//     //         adjustedForward = -forwardCapWhenLateralMovement;
//     //     }
//     // }
//
//     SetSideMovement();
//     SetRingRotationsForForward(adjustedForward);
//     SetTiltForUpAndDown();
// }